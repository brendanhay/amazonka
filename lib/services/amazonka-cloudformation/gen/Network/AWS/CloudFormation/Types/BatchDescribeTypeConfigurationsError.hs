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
-- Module      : Network.AWS.CloudFormation.Types.BatchDescribeTypeConfigurationsError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.BatchDescribeTypeConfigurationsError where

import Network.AWS.CloudFormation.Types.TypeConfigurationIdentifier
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information concerning an error generated during the setting of
-- configuration data for a CloudFormation extension.
--
-- /See:/ 'newBatchDescribeTypeConfigurationsError' smart constructor.
data BatchDescribeTypeConfigurationsError = BatchDescribeTypeConfigurationsError'
  { typeConfigurationIdentifier :: Prelude.Maybe TypeConfigurationIdentifier,
    -- | The error code.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    errorMessage :: Prelude.Maybe Prelude.Text
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
-- 'typeConfigurationIdentifier', 'batchDescribeTypeConfigurationsError_typeConfigurationIdentifier' - Undocumented member.
--
-- 'errorCode', 'batchDescribeTypeConfigurationsError_errorCode' - The error code.
--
-- 'errorMessage', 'batchDescribeTypeConfigurationsError_errorMessage' - The error message.
newBatchDescribeTypeConfigurationsError ::
  BatchDescribeTypeConfigurationsError
newBatchDescribeTypeConfigurationsError =
  BatchDescribeTypeConfigurationsError'
    { typeConfigurationIdentifier =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | Undocumented member.
batchDescribeTypeConfigurationsError_typeConfigurationIdentifier :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe TypeConfigurationIdentifier)
batchDescribeTypeConfigurationsError_typeConfigurationIdentifier = Lens.lens (\BatchDescribeTypeConfigurationsError' {typeConfigurationIdentifier} -> typeConfigurationIdentifier) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {typeConfigurationIdentifier = a} :: BatchDescribeTypeConfigurationsError)

-- | The error code.
batchDescribeTypeConfigurationsError_errorCode :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe Prelude.Text)
batchDescribeTypeConfigurationsError_errorCode = Lens.lens (\BatchDescribeTypeConfigurationsError' {errorCode} -> errorCode) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {errorCode = a} :: BatchDescribeTypeConfigurationsError)

-- | The error message.
batchDescribeTypeConfigurationsError_errorMessage :: Lens.Lens' BatchDescribeTypeConfigurationsError (Prelude.Maybe Prelude.Text)
batchDescribeTypeConfigurationsError_errorMessage = Lens.lens (\BatchDescribeTypeConfigurationsError' {errorMessage} -> errorMessage) (\s@BatchDescribeTypeConfigurationsError' {} a -> s {errorMessage = a} :: BatchDescribeTypeConfigurationsError)

instance
  Core.FromXML
    BatchDescribeTypeConfigurationsError
  where
  parseXML x =
    BatchDescribeTypeConfigurationsError'
      Prelude.<$> (x Core..@? "TypeConfigurationIdentifier")
      Prelude.<*> (x Core..@? "ErrorCode")
      Prelude.<*> (x Core..@? "ErrorMessage")

instance
  Prelude.Hashable
    BatchDescribeTypeConfigurationsError

instance
  Prelude.NFData
    BatchDescribeTypeConfigurationsError
