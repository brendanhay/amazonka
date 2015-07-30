{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current data retrieval policy for the account
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
    , gdrprsPolicy
    , gdrprsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input for GetDataRetrievalPolicy.
--
-- /See:/ 'getDataRetrievalPolicy' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrpAccountId'
newtype GetDataRetrievalPolicy = GetDataRetrievalPolicy'
    { _gdrpAccountId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDataRetrievalPolicy' smart constructor.
getDataRetrievalPolicy :: Text -> GetDataRetrievalPolicy
getDataRetrievalPolicy pAccountId_ =
    GetDataRetrievalPolicy'
    { _gdrpAccountId = pAccountId_
    }

-- | The @AccountId@ value is the AWS account ID. This value must match the
-- AWS account ID associated with the credentials used to sign the request.
-- You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you
-- specify your account ID, do not include any hyphens (apos-apos) in the
-- ID.
gdrpAccountId :: Lens' GetDataRetrievalPolicy Text
gdrpAccountId = lens _gdrpAccountId (\ s a -> s{_gdrpAccountId = a});

instance AWSRequest GetDataRetrievalPolicy where
        type Sv GetDataRetrievalPolicy = Glacier
        type Rs GetDataRetrievalPolicy =
             GetDataRetrievalPolicyResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetDataRetrievalPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance ToHeaders GetDataRetrievalPolicy where
        toHeaders = const mempty

instance ToPath GetDataRetrievalPolicy where
        toPath GetDataRetrievalPolicy'{..}
          = mconcat
              ["/", toBS _gdrpAccountId,
               "/policies/data-retrieval"]

instance ToQuery GetDataRetrievalPolicy where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to the @GetDataRetrievalPolicy@
-- request.
--
-- /See:/ 'getDataRetrievalPolicyResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gdrprsPolicy'
--
-- * 'gdrprsStatus'
data GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'
    { _gdrprsPolicy :: !(Maybe DataRetrievalPolicy)
    , _gdrprsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetDataRetrievalPolicyResponse' smart constructor.
getDataRetrievalPolicyResponse :: Int -> GetDataRetrievalPolicyResponse
getDataRetrievalPolicyResponse pStatus_ =
    GetDataRetrievalPolicyResponse'
    { _gdrprsPolicy = Nothing
    , _gdrprsStatus = pStatus_
    }

-- | Contains the returned data retrieval policy in JSON format.
gdrprsPolicy :: Lens' GetDataRetrievalPolicyResponse (Maybe DataRetrievalPolicy)
gdrprsPolicy = lens _gdrprsPolicy (\ s a -> s{_gdrprsPolicy = a});

-- | FIXME: Undocumented member.
gdrprsStatus :: Lens' GetDataRetrievalPolicyResponse Int
gdrprsStatus = lens _gdrprsStatus (\ s a -> s{_gdrprsStatus = a});
