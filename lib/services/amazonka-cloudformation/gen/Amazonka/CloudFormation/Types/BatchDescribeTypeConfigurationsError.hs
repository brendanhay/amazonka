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
-- Module      : Amazonka.CloudFormation.Types.BatchDescribeTypeConfigurationsError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.BatchDescribeTypeConfigurationsError where

import Amazonka.CloudFormation.Types.TypeConfigurationIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information concerning an error generated during the setting of
-- configuration data for a CloudFormation extension.
--
-- /See:/ 'newBatchDescribeTypeConfigurationsError' smart constructor.
data BatchDescribeTypeConfigurationsError = BatchDescribeTypeConfigurationsError'
  { -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    typeConfigurationIdentifier :: Prelude.Maybe TypeConfigurationIdentifier,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDescribeTypeConfigurationsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'batchDescribeTypeConfigurationsError_errorMessage' - The error message.
--
-- 'typeConfigurationIdentifier', 'batchDescribeTypeConfigurationsError_typeConfigurationIdentifier' - Undocumented member.
--
-- 'errorCode', 'batchDescribeTypeConfigurationsError_errorCode' - The error code.
newBatchDescribeTypeConfigurationsError ::
  BatchDescribeTypeConfigurationsError
newBatchDescribeTypeConfigurationsError =
  BatchDescribeTypeConfigurationsError'
    { errorMessage =
        Prelude.Nothing,
      typeConfigurationIdentifier =
        Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The error message.
batchDescribeTypeConfigurationsError_errorMessage :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe Prelude.Text)
batchDescribeTypeConfigurationsError_errorMessage = Lens.lens (\BatchDescribeTypeConfigurationsError' {errorMessage} -> errorMessage) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {errorMessage = a} :: BatchDescribeTypeConfigurationsError)

-- | Undocumented member.
batchDescribeTypeConfigurationsError_typeConfigurationIdentifier :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe TypeConfigurationIdentifier)
batchDescribeTypeConfigurationsError_typeConfigurationIdentifier = Lens.lens (\BatchDescribeTypeConfigurationsError' {typeConfigurationIdentifier} -> typeConfigurationIdentifier) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {typeConfigurationIdentifier = a} :: BatchDescribeTypeConfigurationsError)

-- | The error code.
batchDescribeTypeConfigurationsError_errorCode :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe Prelude.Text)
batchDescribeTypeConfigurationsError_errorCode = Lens.lens (\BatchDescribeTypeConfigurationsError' {errorCode} -> errorCode) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {errorCode = a} :: BatchDescribeTypeConfigurationsError)

instance
  Data.FromXML
    BatchDescribeTypeConfigurationsError
  where
  parseXML x =
    BatchDescribeTypeConfigurationsError'
      Prelude.<$> (x Data..@? "ErrorMessage")
      Prelude.<*> (x Data..@? "TypeConfigurationIdentifier")
      Prelude.<*> (x Data..@? "ErrorCode")

instance
  Prelude.Hashable
    BatchDescribeTypeConfigurationsError
  where
  hashWithSalt
    _salt
    BatchDescribeTypeConfigurationsError' {..} =
      _salt `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` typeConfigurationIdentifier
        `Prelude.hashWithSalt` errorCode

instance
  Prelude.NFData
    BatchDescribeTypeConfigurationsError
  where
  rnf BatchDescribeTypeConfigurationsError' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf typeConfigurationIdentifier
      `Prelude.seq` Prelude.rnf errorCode
