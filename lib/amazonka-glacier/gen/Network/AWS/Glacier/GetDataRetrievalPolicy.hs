{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current data retrieval policy for the account and region specified in the GET request. For more information about data retrieval policies, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies> .
--
--
module Network.AWS.Glacier.GetDataRetrievalPolicy
    (
    -- * Creating a Request
      getDataRetrievalPolicy
    , GetDataRetrievalPolicy
    -- * Request Lenses
    , gdrpAccountId

    -- * Destructuring the Response
    , getDataRetrievalPolicyResponse
    , GetDataRetrievalPolicyResponse
    -- * Response Lenses
    , gdrprsPolicy
    , gdrprsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input for GetDataRetrievalPolicy.
--
--
--
-- /See:/ 'getDataRetrievalPolicy' smart constructor.
newtype GetDataRetrievalPolicy = GetDataRetrievalPolicy'
  { _gdrpAccountId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataRetrievalPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrpAccountId' - The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
getDataRetrievalPolicy
    :: Text -- ^ 'gdrpAccountId'
    -> GetDataRetrievalPolicy
getDataRetrievalPolicy pAccountId_ =
  GetDataRetrievalPolicy' {_gdrpAccountId = pAccountId_}


-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
gdrpAccountId :: Lens' GetDataRetrievalPolicy Text
gdrpAccountId = lens _gdrpAccountId (\ s a -> s{_gdrpAccountId = a})

instance AWSRequest GetDataRetrievalPolicy where
        type Rs GetDataRetrievalPolicy =
             GetDataRetrievalPolicyResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 GetDataRetrievalPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance Hashable GetDataRetrievalPolicy where

instance NFData GetDataRetrievalPolicy where

instance ToHeaders GetDataRetrievalPolicy where
        toHeaders = const mempty

instance ToPath GetDataRetrievalPolicy where
        toPath GetDataRetrievalPolicy'{..}
          = mconcat
              ["/", toBS _gdrpAccountId,
               "/policies/data-retrieval"]

instance ToQuery GetDataRetrievalPolicy where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to the @GetDataRetrievalPolicy@ request.
--
--
--
-- /See:/ 'getDataRetrievalPolicyResponse' smart constructor.
data GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'
  { _gdrprsPolicy         :: !(Maybe DataRetrievalPolicy)
  , _gdrprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataRetrievalPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrprsPolicy' - Contains the returned data retrieval policy in JSON format.
--
-- * 'gdrprsResponseStatus' - -- | The response status code.
getDataRetrievalPolicyResponse
    :: Int -- ^ 'gdrprsResponseStatus'
    -> GetDataRetrievalPolicyResponse
getDataRetrievalPolicyResponse pResponseStatus_ =
  GetDataRetrievalPolicyResponse'
    {_gdrprsPolicy = Nothing, _gdrprsResponseStatus = pResponseStatus_}


-- | Contains the returned data retrieval policy in JSON format.
gdrprsPolicy :: Lens' GetDataRetrievalPolicyResponse (Maybe DataRetrievalPolicy)
gdrprsPolicy = lens _gdrprsPolicy (\ s a -> s{_gdrprsPolicy = a})

-- | -- | The response status code.
gdrprsResponseStatus :: Lens' GetDataRetrievalPolicyResponse Int
gdrprsResponseStatus = lens _gdrprsResponseStatus (\ s a -> s{_gdrprsResponseStatus = a})

instance NFData GetDataRetrievalPolicyResponse where
