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
-- Module      : Amazonka.Firehose.Types.DeliveryStreamEncryptionConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.DeliveryStreamEncryptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.DeliveryStreamEncryptionStatus
import Amazonka.Firehose.Types.FailureDescription
import Amazonka.Firehose.Types.KeyType
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the server-side encryption (SSE) status for
-- the delivery stream, the type customer master key (CMK) in use, if any,
-- and the ARN of the CMK. You can get
-- @DeliveryStreamEncryptionConfiguration@ by invoking the
-- DescribeDeliveryStream operation.
--
-- /See:/ 'newDeliveryStreamEncryptionConfiguration' smart constructor.
data DeliveryStreamEncryptionConfiguration = DeliveryStreamEncryptionConfiguration'
  { -- | Provides details in case one of the following operations fails due to an
    -- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
    -- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
    failureDescription :: Prelude.Maybe FailureDescription,
    -- | Indicates the type of customer master key (CMK) that is used for
    -- encryption. The default setting is @AWS_OWNED_CMK@. For more information
    -- about CMKs, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
    keyType :: Prelude.Maybe KeyType,
    -- | This is the server-side encryption (SSE) status for the delivery stream.
    -- For a full description of the different values of this status, see
    -- StartDeliveryStreamEncryption and StopDeliveryStreamEncryption. If this
    -- status is @ENABLING_FAILED@ or @DISABLING_FAILED@, it is the status of
    -- the most recent attempt to enable or disable SSE, respectively.
    status :: Prelude.Maybe DeliveryStreamEncryptionStatus,
    -- | If @KeyType@ is @CUSTOMER_MANAGED_CMK@, this field contains the ARN of
    -- the customer managed CMK. If @KeyType@ is @AWS_OWNED_CMK@,
    -- @DeliveryStreamEncryptionConfiguration@ doesn\'t contain a value for
    -- @KeyARN@.
    keyARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeliveryStreamEncryptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureDescription', 'deliveryStreamEncryptionConfiguration_failureDescription' - Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
--
-- 'keyType', 'deliveryStreamEncryptionConfiguration_keyType' - Indicates the type of customer master key (CMK) that is used for
-- encryption. The default setting is @AWS_OWNED_CMK@. For more information
-- about CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
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
newDeliveryStreamEncryptionConfiguration ::
  DeliveryStreamEncryptionConfiguration
newDeliveryStreamEncryptionConfiguration =
  DeliveryStreamEncryptionConfiguration'
    { failureDescription =
        Prelude.Nothing,
      keyType = Prelude.Nothing,
      status = Prelude.Nothing,
      keyARN = Prelude.Nothing
    }

-- | Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
deliveryStreamEncryptionConfiguration_failureDescription :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Prelude.Maybe FailureDescription)
deliveryStreamEncryptionConfiguration_failureDescription = Lens.lens (\DeliveryStreamEncryptionConfiguration' {failureDescription} -> failureDescription) (\s@DeliveryStreamEncryptionConfiguration' {} a -> s {failureDescription = a} :: DeliveryStreamEncryptionConfiguration)

-- | Indicates the type of customer master key (CMK) that is used for
-- encryption. The default setting is @AWS_OWNED_CMK@. For more information
-- about CMKs, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/concepts.html#master_keys Customer Master Keys (CMKs)>.
deliveryStreamEncryptionConfiguration_keyType :: Lens.Lens' DeliveryStreamEncryptionConfiguration (Prelude.Maybe KeyType)
deliveryStreamEncryptionConfiguration_keyType = Lens.lens (\DeliveryStreamEncryptionConfiguration' {keyType} -> keyType) (\s@DeliveryStreamEncryptionConfiguration' {} a -> s {keyType = a} :: DeliveryStreamEncryptionConfiguration)

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

instance
  Data.FromJSON
    DeliveryStreamEncryptionConfiguration
  where
  parseJSON =
    Data.withObject
      "DeliveryStreamEncryptionConfiguration"
      ( \x ->
          DeliveryStreamEncryptionConfiguration'
            Prelude.<$> (x Data..:? "FailureDescription")
            Prelude.<*> (x Data..:? "KeyType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "KeyARN")
      )

instance
  Prelude.Hashable
    DeliveryStreamEncryptionConfiguration
  where
  hashWithSalt
    _salt
    DeliveryStreamEncryptionConfiguration' {..} =
      _salt `Prelude.hashWithSalt` failureDescription
        `Prelude.hashWithSalt` keyType
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` keyARN

instance
  Prelude.NFData
    DeliveryStreamEncryptionConfiguration
  where
  rnf DeliveryStreamEncryptionConfiguration' {..} =
    Prelude.rnf failureDescription
      `Prelude.seq` Prelude.rnf keyType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf keyARN
