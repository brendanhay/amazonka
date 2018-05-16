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
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether automatic rotation of the key material is enabled for the specified customer master key (CMK).
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the KeyId parameter.
--
module Network.AWS.KMS.GetKeyRotationStatus
    (
    -- * Creating a Request
      getKeyRotationStatus
    , GetKeyRotationStatus
    -- * Request Lenses
    , gkrsKeyId

    -- * Destructuring the Response
    , getKeyRotationStatusResponse
    , GetKeyRotationStatusResponse
    -- * Response Lenses
    , gkrsrsKeyRotationEnabled
    , gkrsrsResponseStatus
    ) where

import Network.AWS.KMS.Types
import Network.AWS.KMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyRotationStatus' smart constructor.
newtype GetKeyRotationStatus = GetKeyRotationStatus'
  { _gkrsKeyId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetKeyRotationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkrsKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
getKeyRotationStatus
    :: Text -- ^ 'gkrsKeyId'
    -> GetKeyRotationStatus
getKeyRotationStatus pKeyId_ = GetKeyRotationStatus' {_gkrsKeyId = pKeyId_}


-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
gkrsKeyId :: Lens' GetKeyRotationStatus Text
gkrsKeyId = lens _gkrsKeyId (\ s a -> s{_gkrsKeyId = a})

instance AWSRequest GetKeyRotationStatus where
        type Rs GetKeyRotationStatus =
             GetKeyRotationStatusResponse
        request = postJSON kms
        response
          = receiveJSON
              (\ s h x ->
                 GetKeyRotationStatusResponse' <$>
                   (x .?> "KeyRotationEnabled") <*> (pure (fromEnum s)))

instance Hashable GetKeyRotationStatus where

instance NFData GetKeyRotationStatus where

instance ToHeaders GetKeyRotationStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("TrentService.GetKeyRotationStatus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetKeyRotationStatus where
        toJSON GetKeyRotationStatus'{..}
          = object (catMaybes [Just ("KeyId" .= _gkrsKeyId)])

instance ToPath GetKeyRotationStatus where
        toPath = const "/"

instance ToQuery GetKeyRotationStatus where
        toQuery = const mempty

-- | /See:/ 'getKeyRotationStatusResponse' smart constructor.
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
  { _gkrsrsKeyRotationEnabled :: !(Maybe Bool)
  , _gkrsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetKeyRotationStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkrsrsKeyRotationEnabled' - A Boolean value that specifies whether key rotation is enabled.
--
-- * 'gkrsrsResponseStatus' - -- | The response status code.
getKeyRotationStatusResponse
    :: Int -- ^ 'gkrsrsResponseStatus'
    -> GetKeyRotationStatusResponse
getKeyRotationStatusResponse pResponseStatus_ =
  GetKeyRotationStatusResponse'
    { _gkrsrsKeyRotationEnabled = Nothing
    , _gkrsrsResponseStatus = pResponseStatus_
    }


-- | A Boolean value that specifies whether key rotation is enabled.
gkrsrsKeyRotationEnabled :: Lens' GetKeyRotationStatusResponse (Maybe Bool)
gkrsrsKeyRotationEnabled = lens _gkrsrsKeyRotationEnabled (\ s a -> s{_gkrsrsKeyRotationEnabled = a})

-- | -- | The response status code.
gkrsrsResponseStatus :: Lens' GetKeyRotationStatusResponse Int
gkrsrsResponseStatus = lens _gkrsrsResponseStatus (\ s a -> s{_gkrsrsResponseStatus = a})

instance NFData GetKeyRotationStatusResponse where
