{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
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

-- | This operation returns the current data retrieval policy for the account
-- and region specified in the GET request. For more information about data
-- retrieval policies, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies>.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetDataRetrievalPolicy.html>
module Network.AWS.Glacier.GetDataRetrievalPolicy
    (
    -- * Request
      GetDataRetrievalPolicy
    -- ** Request constructor
    , getDataRetrievalPolicy
    -- ** Request lenses
    , gdrpAccountId

    -- * Response
    , GetDataRetrievalPolicyResponse
    -- ** Response constructor
    , getDataRetrievalPolicyResponse
    -- ** Response lenses
    , gdrprPolicy
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDataRetrievalPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrpAccountId'
newtype GetDataRetrievalPolicy = GetDataRetrievalPolicy'{_gdrpAccountId :: Text} deriving (Eq, Read, Show)

-- | 'GetDataRetrievalPolicy' smart constructor.
getDataRetrievalPolicy :: Text -> GetDataRetrievalPolicy
getDataRetrievalPolicy pAccountId = GetDataRetrievalPolicy'{_gdrpAccountId = pAccountId};

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your Account ID, do not include any hyphens (apos-apos) in the
-- ID.
gdrpAccountId :: Lens' GetDataRetrievalPolicy Text
gdrpAccountId = lens _gdrpAccountId (\ s a -> s{_gdrpAccountId = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetDataRetrievalPolicy where
        type Sv GetDataRetrievalPolicy = Glacier
        type Rs GetDataRetrievalPolicy =
             GetDataRetrievalPolicyResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetDataRetrievalPolicyResponse' <$> (x .?> "Policy"))

instance ToHeaders GetDataRetrievalPolicy where
        toHeaders = const mempty

instance ToPath GetDataRetrievalPolicy where
        toPath GetDataRetrievalPolicy'{..}
          = mconcat
              ["/", toText _gdrpAccountId,
               "/policies/data-retrieval"]

instance ToQuery GetDataRetrievalPolicy where
        toQuery = const mempty

-- | /See:/ 'getDataRetrievalPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrprPolicy'
newtype GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'{_gdrprPolicy :: Maybe DataRetrievalPolicy} deriving (Eq, Read, Show)

-- | 'GetDataRetrievalPolicyResponse' smart constructor.
getDataRetrievalPolicyResponse :: GetDataRetrievalPolicyResponse
getDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'{_gdrprPolicy = Nothing};

-- | Contains the returned data retrieval policy in JSON format.
gdrprPolicy :: Lens' GetDataRetrievalPolicyResponse (Maybe DataRetrievalPolicy)
gdrprPolicy = lens _gdrprPolicy (\ s a -> s{_gdrprPolicy = a});
