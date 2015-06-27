{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Glacier.SetDataRetrievalPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation sets and then enacts a data retrieval policy in the
-- region specified in the PUT request. You can set one policy per region
-- for an AWS account. The policy is enacted within a few minutes of a
-- successful PUT operation.
--
-- The set policy operation does not affect retrieval jobs that were in
-- progress before the policy was enacted. For more information about data
-- retrieval policies, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetDataRetrievalPolicy.html>
module Network.AWS.Glacier.SetDataRetrievalPolicy
    (
    -- * Request
      SetDataRetrievalPolicy
    -- ** Request constructor
    , setDataRetrievalPolicy
    -- ** Request lenses
    , sdrpPolicy
    , sdrpAccountId

    -- * Response
    , SetDataRetrievalPolicyResponse
    -- ** Response constructor
    , setDataRetrievalPolicyResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | SetDataRetrievalPolicy input.
--
-- /See:/ 'setDataRetrievalPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sdrpPolicy'
--
-- * 'sdrpAccountId'
data SetDataRetrievalPolicy = SetDataRetrievalPolicy'
    { _sdrpPolicy    :: Maybe DataRetrievalPolicy
    , _sdrpAccountId :: Text
    } deriving (Eq,Read,Show)

-- | 'SetDataRetrievalPolicy' smart constructor.
setDataRetrievalPolicy :: Text -> SetDataRetrievalPolicy
setDataRetrievalPolicy pAccountId =
    SetDataRetrievalPolicy'
    { _sdrpPolicy = Nothing
    , _sdrpAccountId = pAccountId
    }

-- | The data retrieval policy in JSON format.
sdrpPolicy :: Lens' SetDataRetrievalPolicy (Maybe DataRetrievalPolicy)
sdrpPolicy = lens _sdrpPolicy (\ s a -> s{_sdrpPolicy = a});

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your Account ID, do not include any hyphens (apos-apos) in the
-- ID.
sdrpAccountId :: Lens' SetDataRetrievalPolicy Text
sdrpAccountId = lens _sdrpAccountId (\ s a -> s{_sdrpAccountId = a});

instance AWSRequest SetDataRetrievalPolicy where
        type Sv SetDataRetrievalPolicy = Glacier
        type Rs SetDataRetrievalPolicy =
             SetDataRetrievalPolicyResponse
        request = putJSON
        response
          = receiveNull SetDataRetrievalPolicyResponse'

instance ToHeaders SetDataRetrievalPolicy where
        toHeaders = const mempty

instance ToJSON SetDataRetrievalPolicy where
        toJSON SetDataRetrievalPolicy'{..}
          = object ["Policy" .= _sdrpPolicy]

instance ToPath SetDataRetrievalPolicy where
        toPath SetDataRetrievalPolicy'{..}
          = mconcat
              ["/", toText _sdrpAccountId,
               "/policies/data-retrieval"]

instance ToQuery SetDataRetrievalPolicy where
        toQuery = const mempty

-- | /See:/ 'setDataRetrievalPolicyResponse' smart constructor.
data SetDataRetrievalPolicyResponse =
    SetDataRetrievalPolicyResponse'
    deriving (Eq,Read,Show)

-- | 'SetDataRetrievalPolicyResponse' smart constructor.
setDataRetrievalPolicyResponse :: SetDataRetrievalPolicyResponse
setDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'
