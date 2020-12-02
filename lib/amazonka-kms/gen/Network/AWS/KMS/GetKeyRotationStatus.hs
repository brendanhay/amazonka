{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.GetKeyRotationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Boolean value that indicates whether <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> is enabled for the specified customer master key (CMK).
--
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . The key rotation status for these CMKs is always @false@ .
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
--
--     * Disabled: The key rotation status does not change when you disable a CMK. However, while the CMK is disabled, AWS KMS does not rotate the backing key.
--
--     * Pending deletion: While a CMK is pending deletion, its key rotation status is @false@ and AWS KMS does not rotate the backing key. If you cancel the deletion, the original key rotation status is restored.
--
--
--
-- To perform this operation on a CMK in a different AWS account, specify the key ARN in the value of the @KeyId@ parameter.
module Network.AWS.KMS.GetKeyRotationStatus
  ( -- * Creating a Request
    getKeyRotationStatus,
    GetKeyRotationStatus,

    -- * Request Lenses
    gkrsKeyId,

    -- * Destructuring the Response
    getKeyRotationStatusResponse,
    GetKeyRotationStatusResponse,

    -- * Response Lenses
    gkrsrsKeyRotationEnabled,
    gkrsrsResponseStatus,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyRotationStatus' smart constructor.
newtype GetKeyRotationStatus = GetKeyRotationStatus'
  { _gkrsKeyId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyRotationStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkrsKeyId' - A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
getKeyRotationStatus ::
  -- | 'gkrsKeyId'
  Text ->
  GetKeyRotationStatus
getKeyRotationStatus pKeyId_ =
  GetKeyRotationStatus' {_gkrsKeyId = pKeyId_}

-- | A unique identifier for the customer master key (CMK). Specify the key ID or the Amazon Resource Name (ARN) of the CMK. To specify a CMK in a different AWS account, you must use the key ARN. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
gkrsKeyId :: Lens' GetKeyRotationStatus Text
gkrsKeyId = lens _gkrsKeyId (\s a -> s {_gkrsKeyId = a})

instance AWSRequest GetKeyRotationStatus where
  type Rs GetKeyRotationStatus = GetKeyRotationStatusResponse
  request = postJSON kms
  response =
    receiveJSON
      ( \s h x ->
          GetKeyRotationStatusResponse'
            <$> (x .?> "KeyRotationEnabled") <*> (pure (fromEnum s))
      )

instance Hashable GetKeyRotationStatus

instance NFData GetKeyRotationStatus

instance ToHeaders GetKeyRotationStatus where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.GetKeyRotationStatus" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetKeyRotationStatus where
  toJSON GetKeyRotationStatus' {..} =
    object (catMaybes [Just ("KeyId" .= _gkrsKeyId)])

instance ToPath GetKeyRotationStatus where
  toPath = const "/"

instance ToQuery GetKeyRotationStatus where
  toQuery = const mempty

-- | /See:/ 'getKeyRotationStatusResponse' smart constructor.
data GetKeyRotationStatusResponse = GetKeyRotationStatusResponse'
  { _gkrsrsKeyRotationEnabled ::
      !(Maybe Bool),
    _gkrsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyRotationStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkrsrsKeyRotationEnabled' - A Boolean value that specifies whether key rotation is enabled.
--
-- * 'gkrsrsResponseStatus' - -- | The response status code.
getKeyRotationStatusResponse ::
  -- | 'gkrsrsResponseStatus'
  Int ->
  GetKeyRotationStatusResponse
getKeyRotationStatusResponse pResponseStatus_ =
  GetKeyRotationStatusResponse'
    { _gkrsrsKeyRotationEnabled =
        Nothing,
      _gkrsrsResponseStatus = pResponseStatus_
    }

-- | A Boolean value that specifies whether key rotation is enabled.
gkrsrsKeyRotationEnabled :: Lens' GetKeyRotationStatusResponse (Maybe Bool)
gkrsrsKeyRotationEnabled = lens _gkrsrsKeyRotationEnabled (\s a -> s {_gkrsrsKeyRotationEnabled = a})

-- | -- | The response status code.
gkrsrsResponseStatus :: Lens' GetKeyRotationStatusResponse Int
gkrsrsResponseStatus = lens _gkrsrsResponseStatus (\s a -> s {_gkrsrsResponseStatus = a})

instance NFData GetKeyRotationStatusResponse
