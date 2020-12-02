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
-- Module      : Network.AWS.KMS.DisableKeyRotation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables <https://docs.aws.amazon.com/kms/latest/developerguide/rotate-keys.html automatic rotation of the key material> for the specified symmetric customer master key (CMK).
--
--
-- You cannot enable automatic rotation of asymmetric CMKs, CMKs with imported key material, or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . You cannot perform this operation on a CMK in a different AWS account.
--
-- The CMK that you use for this operation must be in a compatible key state. For details, see <https://docs.aws.amazon.com/kms/latest/developerguide/key-state.html How Key State Affects Use of a Customer Master Key> in the /AWS Key Management Service Developer Guide/ .
module Network.AWS.KMS.DisableKeyRotation
  ( -- * Creating a Request
    disableKeyRotation,
    DisableKeyRotation,

    -- * Request Lenses
    dkrKeyId,

    -- * Destructuring the Response
    disableKeyRotationResponse,
    DisableKeyRotationResponse,
  )
where

import Network.AWS.KMS.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableKeyRotation' smart constructor.
newtype DisableKeyRotation = DisableKeyRotation' {_dkrKeyId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableKeyRotation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkrKeyId' - Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs> , CMKs with <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
disableKeyRotation ::
  -- | 'dkrKeyId'
  Text ->
  DisableKeyRotation
disableKeyRotation pKeyId_ =
  DisableKeyRotation' {_dkrKeyId = pKeyId_}

-- | Identifies a symmetric customer master key (CMK). You cannot enable automatic rotation of <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html#asymmetric-cmks asymmetric CMKs> , CMKs with <https://docs.aws.amazon.com/kms/latest/developerguide/importing-keys.html imported key material> , or CMKs in a <https://docs.aws.amazon.com/kms/latest/developerguide/custom-key-store-overview.html custom key store> . Specify the key ID or the Amazon Resource Name (ARN) of the CMK. For example:     * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-east-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@  To get the key ID and key ARN for a CMK, use 'ListKeys' or 'DescribeKey' .
dkrKeyId :: Lens' DisableKeyRotation Text
dkrKeyId = lens _dkrKeyId (\s a -> s {_dkrKeyId = a})

instance AWSRequest DisableKeyRotation where
  type Rs DisableKeyRotation = DisableKeyRotationResponse
  request = postJSON kms
  response = receiveNull DisableKeyRotationResponse'

instance Hashable DisableKeyRotation

instance NFData DisableKeyRotation

instance ToHeaders DisableKeyRotation where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("TrentService.DisableKeyRotation" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DisableKeyRotation where
  toJSON DisableKeyRotation' {..} =
    object (catMaybes [Just ("KeyId" .= _dkrKeyId)])

instance ToPath DisableKeyRotation where
  toPath = const "/"

instance ToQuery DisableKeyRotation where
  toQuery = const mempty

-- | /See:/ 'disableKeyRotationResponse' smart constructor.
data DisableKeyRotationResponse = DisableKeyRotationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableKeyRotationResponse' with the minimum fields required to make a request.
disableKeyRotationResponse ::
  DisableKeyRotationResponse
disableKeyRotationResponse = DisableKeyRotationResponse'

instance NFData DisableKeyRotationResponse
