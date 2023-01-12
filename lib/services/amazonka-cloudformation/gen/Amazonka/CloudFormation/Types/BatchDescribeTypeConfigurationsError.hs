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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text,
    typeConfigurationIdentifier :: Prelude.Maybe TypeConfigurationIdentifier
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
-- 'errorCode', 'batchDescribeTypeConfigurationsError_errorCode' - The error code.
--
-- 'errorMessage', 'batchDescribeTypeConfigurationsError_errorMessage' - The error message.
--
-- 'typeConfigurationIdentifier', 'batchDescribeTypeConfigurationsError_typeConfigurationIdentifier' - Undocumented member.
newBatchDescribeTypeConfigurationsError ::
  BatchDescribeTypeConfigurationsError
newBatchDescribeTypeConfigurationsError =
  BatchDescribeTypeConfigurationsError'
    { errorCode =
        Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      typeConfigurationIdentifier =
        Prelude.Nothing
    }

-- | The error code.
batchDescribeTypeConfigurationsError_errorCode :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe Prelude.Text)
batchDescribeTypeConfigurationsError_errorCode = Lens.lens (\BatchDescribeTypeConfigurationsError' {errorCode} -> errorCode) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {errorCode = a} :: BatchDescribeTypeConfigurationsError)

-- | The error message.
batchDescribeTypeConfigurationsError_errorMessage :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe Prelude.Text)
batchDescribeTypeConfigurationsError_errorMessage = Lens.lens (\BatchDescribeTypeConfigurationsError' {errorMessage} -> errorMessage) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {errorMessage = a} :: BatchDescribeTypeConfigurationsError)

-- | Undocumented member.
batchDescribeTypeConfigurationsError_typeConfigurationIdentifier :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe TypeConfigurationIdentifier)
batchDescribeTypeConfigurationsError_typeConfigurationIdentifier = Lens.lens (\BatchDescribeTypeConfigurationsError' {typeConfigurationIdentifier} -> typeConfigurationIdentifier) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {typeConfigurationIdentifier = a} :: BatchDescribeTypeConfigurationsError)

instance
  Data.FromXML
    BatchDescribeTypeConfigurationsError
  where
  parseXML x =
    BatchDescribeTypeConfigurationsError'
      Prelude.<$> (x Data..@? "ErrorCode")
      Prelude.<*> (x Data..@? "ErrorMessage")
      Prelude.<*> (x Data..@? "TypeConfigurationIdentifier")

instance
  Prelude.Hashable
    BatchDescribeTypeConfigurationsError
  where
  hashWithSalt
    _salt
    BatchDescribeTypeConfigurationsError' {..} =
      _salt `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` errorMessage
        `Prelude.hashWithSalt` typeConfigurationIdentifier

instance
  Prelude.NFData
    BatchDescribeTypeConfigurationsError
  where
  rnf BatchDescribeTypeConfigurationsError' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf typeConfigurationIdentifier
