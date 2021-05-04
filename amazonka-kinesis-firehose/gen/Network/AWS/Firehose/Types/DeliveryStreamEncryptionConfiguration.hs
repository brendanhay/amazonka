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
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration where

import Network.AWS.Firehose.Types.DeliveryStreamEncryptionStatus
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.KeyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the server-side encryption (SSE) status for
-- the delivery stream, the type customer master key (CMK) in use, if any,
-- and the ARN of the CMK. You can get
-- @DeliveryStreamEncryptionConfiguration@ by invoking the
-- DescribeDeliveryStream operation.
--
-- /See:/ 'newDeliveryStreamEncryptionConfiguration' smart constructor.
data DeliveryStreamEncryptionConfiguration = DeliveryStreamEncryptionConfiguration'
  { -- | This is the server-side encryption (SSE) status for the delivery stream.
    -- For a full description of the different values of this status, see
    -- StartDeliveryStreamEncryption and StopDeliveryStreamEncryption. If this
    -- status is @ENABLING_FAILED@ or @DISABLING_FAILED@, it is the status of
    -- the most recent attempt to enable or disable SSE, respectively.
    status :: Prelude.Maybe DeliveryStreamEncryptionStatus,
    -- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@, this field contains the ARN of
    -- the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@,
    -- @DeliveryStreamEncryptionConfiguration@ doesn\'t contain a value for
    -- @KeyARN@.
    keyARN :: Prelude.Maybe Prelude.Text,
    -- | Indicates the type of customer master key (CMK) that is used for
    -- encryption. The default setting is @AWS_OWNED_CMK@. For more information
    -- about CMKs, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
    keyType :: Prelude.Maybe KeyType,
    -- | Provides details in case one of the following operations fails due to an
    -- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
    -- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
    failureDescription :: Prelude.Maybe FailureDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeliveryStreamEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'deliveryStreamEncryptionConfiguration_status' - This is the server-side encryption (SSE) status for the delivery stream.
-- For a full description of the different values of this status, see
-- StartDeliveryStreamEncryption and StopDeliveryStreamEncryption. If this
-- status is @ENABLING_FAILED@ or @DISABLING_FAILED@, it is the status of
-- the most recent attempt to enable or disable SSE, respectively.
--
-- 'keyARN', 'deliveryStreamEncryptionConfiguration_keyARN' - If @KeyType@ is @CUSTOMER_MANAGED_CMK@, this field contains the ARN of
-- the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@,
-- @DeliveryStreamEncryptionConfiguration@ doesn\'t contain a value for
-- @KeyARN@.
--
-- 'keyType', 'deliveryStreamEncryptionConfiguration_keyType' - Indicates the type of customer master key (CMK) that is used for
-- encryption. The default setting is @AWS_OWNED_CMK@. For more information
-- about CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
--
-- 'failureDescription', 'deliveryStreamEncryptionConfiguration_failureDescription' - Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
newDeliveryStreamEncryptionConfiguration ::
  DeliveryStreamEncryptionConfiguration
newDeliveryStreamEncryptionConfiguration =
  DeliveryStreamEncryptionConfiguration'
    { status =
        Prelude.Nothing,
      keyARN = Prelude.Nothing,
      keyType = Prelude.Nothing,
      failureDescription = Prelude.Nothing
    }

-- | This is the server-side encryption (SSE) status for the delivery stream.
-- For a full description of the different values of this status, see
-- StartDeliveryStreamEncryption and StopDeliveryStreamEncryption. If this
-- status is @ENABLING_FAILED@ or @DISABLING_FAILED@, it is the status of
-- the most recent attempt to enable or disable SSE, respectively.
deliveryStreamEncryptionConfiguration_status :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Prelude.Maybe DeliveryStreamEncryptionStatus)
deliveryStreamEncryptionConfiguration_status = Lens.lens (\DeliveryStreamEncryptionConfiguration' {status} -> status) (\s@DeliveryStreamEncryptionConfiguration' {} a -> s {status = a} :: DeliveryStreamEncryptionConfiguration)

-- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@, this field contains the ARN of
-- the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@,
-- @DeliveryStreamEncryptionConfiguration@ doesn\'t contain a value for
-- @KeyARN@.
deliveryStreamEncryptionConfiguration_keyARN :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Prelude.Maybe Prelude.Text)
deliveryStreamEncryptionConfiguration_keyARN = Lens.lens (\DeliveryStreamEncryptionConfiguration' {keyARN} -> keyARN) (\s@DeliveryStreamEncryptionConfiguration' {} a -> s {keyARN = a} :: DeliveryStreamEncryptionConfiguration)

-- | Indicates the type of customer master key (CMK) that is used for
-- encryption. The default setting is @AWS_OWNED_CMK@. For more information
-- about CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
deliveryStreamEncryptionConfiguration_keyType :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Prelude.Maybe KeyType)
deliveryStreamEncryptionConfiguration_keyType = Lens.lens (\DeliveryStreamEncryptionConfiguration' {keyType} -> keyType) (\s@DeliveryStreamEncryptionConfiguration' {} a -> s {keyType = a} :: DeliveryStreamEncryptionConfiguration)

-- | Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
deliveryStreamEncryptionConfiguration_failureDescription :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Prelude.Maybe FailureDescription)
deliveryStreamEncryptionConfiguration_failureDescription = Lens.lens (\DeliveryStreamEncryptionConfiguration' {failureDescription} -> failureDescription) (\s@DeliveryStreamEncryptionConfiguration' {} a -> s {failureDescription = a} :: DeliveryStreamEncryptionConfiguration)

instance
  Prelude.FromJSON
    DeliveryStreamEncryptionConfiguration
  where
  parseJSON =
    Prelude.withObject
      "DeliveryStreamEncryptionConfiguration"
      ( \x ->
          DeliveryStreamEncryptionConfiguration'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "KeyARN")
            Prelude.<*> (x Prelude..:? "KeyType")
            Prelude.<*> (x Prelude..:? "FailureDescription")
      )

instance
  Prelude.Hashable
    DeliveryStreamEncryptionConfiguration

instance
  Prelude.NFData
    DeliveryStreamEncryptionConfiguration
