{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfigurationInput where

import Network.AWS.Firehose.Types.KeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the type and Amazon Resource Name (ARN) of the CMK to use for
-- Server-Side Encryption (SSE).
--
-- /See:/ 'newDeliveryStreamEncryptionConfigurationInput' smart constructor.
data DeliveryStreamEncryptionConfigurationInput = DeliveryStreamEncryptionConfigurationInput'
  { -- | If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@, you must specify the
    -- Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to
    -- @AWS_OWNED_CMK@, Kinesis Data Firehose uses a service-account CMK.
    keyARN :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of customer master key (CMK) to use for encryption.
    -- The default setting is @AWS_OWNED_CMK@. For more information about CMKs,
    -- see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
    -- When you invoke CreateDeliveryStream or StartDeliveryStreamEncryption
    -- with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose
    -- invokes the Amazon KMS operation
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant>
    -- to create a grant that allows the Kinesis Data Firehose service to use
    -- the customer managed CMK to perform encryption and decryption. Kinesis
    -- Data Firehose manages that grant.
    --
    -- When you invoke StartDeliveryStreamEncryption to change the CMK for a
    -- delivery stream that is encrypted with a customer managed CMK, Kinesis
    -- Data Firehose schedules the grant it had on the old CMK for retirement.
    --
    -- You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500
    -- delivery streams. If a CreateDeliveryStream or
    -- StartDeliveryStreamEncryption operation exceeds this limit, Kinesis Data
    -- Firehose throws a @LimitExceededException@.
    --
    -- To encrypt your delivery stream, use symmetric CMKs. Kinesis Data
    -- Firehose doesn\'t support asymmetric CMKs. For information about
    -- symmetric and asymmetric CMKs, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs>
    -- in the AWS Key Management Service developer guide.
    keyType :: KeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeliveryStreamEncryptionConfigurationInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyARN', 'deliveryStreamEncryptionConfigurationInput_keyARN' - If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@, you must specify the
-- Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to
-- @AWS_OWNED_CMK@, Kinesis Data Firehose uses a service-account CMK.
--
-- 'keyType', 'deliveryStreamEncryptionConfigurationInput_keyType' - Indicates the type of customer master key (CMK) to use for encryption.
-- The default setting is @AWS_OWNED_CMK@. For more information about CMKs,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
-- When you invoke CreateDeliveryStream or StartDeliveryStreamEncryption
-- with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose
-- invokes the Amazon KMS operation
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant>
-- to create a grant that allows the Kinesis Data Firehose service to use
-- the customer managed CMK to perform encryption and decryption. Kinesis
-- Data Firehose manages that grant.
--
-- When you invoke StartDeliveryStreamEncryption to change the CMK for a
-- delivery stream that is encrypted with a customer managed CMK, Kinesis
-- Data Firehose schedules the grant it had on the old CMK for retirement.
--
-- You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500
-- delivery streams. If a CreateDeliveryStream or
-- StartDeliveryStreamEncryption operation exceeds this limit, Kinesis Data
-- Firehose throws a @LimitExceededException@.
--
-- To encrypt your delivery stream, use symmetric CMKs. Kinesis Data
-- Firehose doesn\'t support asymmetric CMKs. For information about
-- symmetric and asymmetric CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs>
-- in the AWS Key Management Service developer guide.
newDeliveryStreamEncryptionConfigurationInput ::
  -- | 'keyType'
  KeyType ->
  DeliveryStreamEncryptionConfigurationInput
newDeliveryStreamEncryptionConfigurationInput
  pKeyType_ =
    DeliveryStreamEncryptionConfigurationInput'
      { keyARN =
          Prelude.Nothing,
        keyType = pKeyType_
      }

-- | If you set @KeyType@ to @CUSTOMER_MANAGED_CMK@, you must specify the
-- Amazon Resource Name (ARN) of the CMK. If you set @KeyType@ to
-- @AWS_OWNED_CMK@, Kinesis Data Firehose uses a service-account CMK.
deliveryStreamEncryptionConfigurationInput_keyARN :: Lens.Lens' DeliveryStreamEncryptionConfigurationInput (Prelude.Maybe Prelude.Text)
deliveryStreamEncryptionConfigurationInput_keyARN = Lens.lens (\DeliveryStreamEncryptionConfigurationInput' {keyARN} -> keyARN) (\s@DeliveryStreamEncryptionConfigurationInput' {} a -> s {keyARN = a} :: DeliveryStreamEncryptionConfigurationInput)

-- | Indicates the type of customer master key (CMK) to use for encryption.
-- The default setting is @AWS_OWNED_CMK@. For more information about CMKs,
-- see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
-- When you invoke CreateDeliveryStream or StartDeliveryStreamEncryption
-- with @KeyType@ set to CUSTOMER_MANAGED_CMK, Kinesis Data Firehose
-- invokes the Amazon KMS operation
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_CreateGrant.html CreateGrant>
-- to create a grant that allows the Kinesis Data Firehose service to use
-- the customer managed CMK to perform encryption and decryption. Kinesis
-- Data Firehose manages that grant.
--
-- When you invoke StartDeliveryStreamEncryption to change the CMK for a
-- delivery stream that is encrypted with a customer managed CMK, Kinesis
-- Data Firehose schedules the grant it had on the old CMK for retirement.
--
-- You can use a CMK of type CUSTOMER_MANAGED_CMK to encrypt up to 500
-- delivery streams. If a CreateDeliveryStream or
-- StartDeliveryStreamEncryption operation exceeds this limit, Kinesis Data
-- Firehose throws a @LimitExceededException@.
--
-- To encrypt your delivery stream, use symmetric CMKs. Kinesis Data
-- Firehose doesn\'t support asymmetric CMKs. For information about
-- symmetric and asymmetric CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symm-asymm-concepts.html About Symmetric and Asymmetric CMKs>
-- in the AWS Key Management Service developer guide.
deliveryStreamEncryptionConfigurationInput_keyType :: Lens.Lens' DeliveryStreamEncryptionConfigurationInput KeyType
deliveryStreamEncryptionConfigurationInput_keyType = Lens.lens (\DeliveryStreamEncryptionConfigurationInput' {keyType} -> keyType) (\s@DeliveryStreamEncryptionConfigurationInput' {} a -> s {keyType = a} :: DeliveryStreamEncryptionConfigurationInput)

instance
  Prelude.Hashable
    DeliveryStreamEncryptionConfigurationInput

instance
  Prelude.NFData
    DeliveryStreamEncryptionConfigurationInput

instance
  Prelude.ToJSON
    DeliveryStreamEncryptionConfigurationInput
  where
  toJSON
    DeliveryStreamEncryptionConfigurationInput' {..} =
      Prelude.object
        ( Prelude.catMaybes
            [ ("KeyARN" Prelude..=) Prelude.<$> keyARN,
              Prelude.Just ("KeyType" Prelude..= keyType)
            ]
        )
