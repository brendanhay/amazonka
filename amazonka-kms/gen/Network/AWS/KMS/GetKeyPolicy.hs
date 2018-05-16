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
-- Module      : Network.AWS.KMS.GetKeyPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a key policy attached to the specified customer master key (CMK). You cannot perform this operation on a CMK in a different AWS account.
--
--
module Network.AWS.KMS.GetKeyPolicy
    (
    -- * Creating a Request
      getKeyPolicy
    , GetKeyPolicy
    -- * Request Lenses
    , gkpKeyId
    , gkpPolicyName

    -- * Destructuring the Response
    , getKeyPolicyResponse
    , GetKeyPolicyResponse
    -- * Response Lenses
    , gkprsPolicy
    , gkprsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyPolicy' smart constructor.
data GetKeyPolicy = GetKeyPolicy'
  { _gkpKeyId      :: !Text
  , _gkpPolicyName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetKeyPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkpKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
--
-- * 'gkpPolicyName' - Specifies the name of the key policy. The only valid name is @default@ . To get the names of key policies, use 'ListKeyPolicies' .
getKeyPolicy
    :: Text -- ^ 'gkpKeyId'
    -> Text -- ^ 'gkpPolicyName'
    -> GetKeyPolicy
getKeyPolicy pKeyId_ pPolicyName_ =
  GetKeyPolicy' {_gkpKeyId = pKeyId_, _gkpPolicyName = pPolicyName_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
gkpKeyId :: Lens' GetKeyPolicy Text
gkpKeyId = lens _gkpKeyId (\ s a -> s{_gkpKeyId = a})

-- | Specifies the name of the key policy. The only valid name is @default@ . To get the names of key policies, use 'ListKeyPolicies' .
gkpPolicyName :: Lens' GetKeyPolicy Text
gkpPolicyName = lens _gkpPolicyName (\ s a -> s{_gkpPolicyName = a})

instance AWSRequest GetKeyPolicy where
        type Rs GetKeyPolicy = GetKeyPolicyResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 GetKeyPolicyResponse' <$>
                   (x .?> "Policy") <*> (pure (fromEnum s)))

instance Hashable GetKeyPolicy where

instance NFData GetKeyPolicy where

instance ToHeaders GetKeyPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GetKeyPolicy" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetKeyPolicy where
        toJSON GetKeyPolicy'{..}
          = object
              (catMaybes
                 [Just ("KeyId" .= _gkpKeyId),
                  Just ("PolicyName" .= _gkpPolicyName)])

instance ToPath GetKeyPolicy where
        toPath = const "/"

instance ToQuery GetKeyPolicy where
        toQuery = const mempty

-- | /See:/ 'getKeyPolicyResponse' smart constructor.
data GetKeyPolicyResponse = GetKeyPolicyResponse'
  { _gkprsPolicy         :: !(Maybe Text)
  , _gkprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetKeyPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkprsPolicy' - A key policy document in JSON format.
--
-- * 'gkprsResponseStatus' - -- | The response status code.
getKeyPolicyResponse
    :: Int -- ^ 'gkprsResponseStatus'
    -> GetKeyPolicyResponse
getKeyPolicyResponse pResponseStatus_ =
  GetKeyPolicyResponse'
    {_gkprsPolicy = Nothing, _gkprsResponseStatus = pResponseStatus_}


-- | A key policy document in JSON format.
gkprsPolicy :: Lens' GetKeyPolicyResponse (Maybe Text)
gkprsPolicy = lens _gkprsPolicy (\ s a -> s{_gkprsPolicy = a})

-- | -- | The response status code.
gkprsResponseStatus :: Lens' GetKeyPolicyResponse Int
gkprsResponseStatus = lens _gkprsResponseStatus (\ s a -> s{_gkprsResponseStatus = a})

instance NFData GetKeyPolicyResponse where
