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
-- Module      : Amazonka.Firehose.Types.FailureDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Firehose.Types.FailureDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Firehose.Types.DeliveryStreamFailureType
import qualified Amazonka.Prelude as Prelude

-- | Provides details in case one of the following operations fails due to an
-- error related to KMS: CreateDeliveryStream, DeleteDeliveryStream,
-- StartDeliveryStreamEncryption, StopDeliveryStreamEncryption.
--
-- /See:/ 'newFailureDescription' smart constructor.
data FailureDescription = FailureDescription'
  { -- | The type of error that caused the failure.
    type' :: DeliveryStreamFailureType,
    -- | A message providing details about the error that caused the failure.
    details :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'failureDescription_type' - The type of error that caused the failure.
--
-- 'details', 'failureDescription_details' - A message providing details about the error that caused the failure.
newFailureDescription ::
  -- | 'type''
  DeliveryStreamFailureType ->
  -- | 'details'
  Prelude.Text ->
  FailureDescription
newFailureDescription pType_ pDetails_ =
  FailureDescription'
    { type' = pType_,
      details = pDetails_
    }

-- | The type of error that caused the failure.
failureDescription_type :: Lens.Lens' FailureDescription DeliveryStreamFailureType
failureDescription_type = Lens.lens (\FailureDescription' {type'} -> type') (\s@FailureDescription' {} a -> s {type' = a} :: FailureDescription)

-- | A message providing details about the error that caused the failure.
failureDescription_details :: Lens.Lens' FailureDescription Prelude.Text
failureDescription_details = Lens.lens (\FailureDescription' {details} -> details) (\s@FailureDescription' {} a -> s {details = a} :: FailureDescription)

instance Data.FromJSON FailureDescription where
  parseJSON =
    Data.withObject
      "FailureDescription"
      ( \x ->
          FailureDescription'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..: "Details")
      )

instance Prelude.Hashable FailureDescription where
  hashWithSalt _salt FailureDescription' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` details

instance Prelude.NFData FailureDescription where
  rnf FailureDescription' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf details
