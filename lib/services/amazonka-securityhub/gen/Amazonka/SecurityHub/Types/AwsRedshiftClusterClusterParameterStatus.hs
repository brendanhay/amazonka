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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a parameter in a cluster parameter group for an Amazon
-- Redshift cluster.
--
-- /See:/ 'newAwsRedshiftClusterClusterParameterStatus' smart constructor.
data AwsRedshiftClusterClusterParameterStatus = AwsRedshiftClusterClusterParameterStatus'
  { -- | The error that prevented the parameter from being applied to the
    -- database.
    parameterApplyErrorDescription :: Prelude.Maybe Prelude.Text,
    -- | The status of the parameter. Indicates whether the parameter is in sync
    -- with the database, waiting for a cluster reboot, or encountered an error
    -- when it was applied.
    --
    -- Valid values: @in-sync@ | @pending-reboot@ | @applying@ |
    -- @invalid-parameter@ | @apply-deferred@ | @apply-error@ | @unknown-error@
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterClusterParameterStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterApplyErrorDescription', 'awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription' - The error that prevented the parameter from being applied to the
-- database.
--
-- 'parameterApplyStatus', 'awsRedshiftClusterClusterParameterStatus_parameterApplyStatus' - The status of the parameter. Indicates whether the parameter is in sync
-- with the database, waiting for a cluster reboot, or encountered an error
-- when it was applied.
--
-- Valid values: @in-sync@ | @pending-reboot@ | @applying@ |
-- @invalid-parameter@ | @apply-deferred@ | @apply-error@ | @unknown-error@
--
-- 'parameterName', 'awsRedshiftClusterClusterParameterStatus_parameterName' - The name of the parameter.
newAwsRedshiftClusterClusterParameterStatus ::
  AwsRedshiftClusterClusterParameterStatus
newAwsRedshiftClusterClusterParameterStatus =
  AwsRedshiftClusterClusterParameterStatus'
    { parameterApplyErrorDescription =
        Prelude.Nothing,
      parameterApplyStatus =
        Prelude.Nothing,
      parameterName = Prelude.Nothing
    }

-- | The error that prevented the parameter from being applied to the
-- database.
awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription :: Lens.Lens' AwsRedshiftClusterClusterParameterStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterParameterStatus_parameterApplyErrorDescription = Lens.lens (\AwsRedshiftClusterClusterParameterStatus' {parameterApplyErrorDescription} -> parameterApplyErrorDescription) (\s@AwsRedshiftClusterClusterParameterStatus' {} a -> s {parameterApplyErrorDescription = a} :: AwsRedshiftClusterClusterParameterStatus)

-- | The status of the parameter. Indicates whether the parameter is in sync
-- with the database, waiting for a cluster reboot, or encountered an error
-- when it was applied.
--
-- Valid values: @in-sync@ | @pending-reboot@ | @applying@ |
-- @invalid-parameter@ | @apply-deferred@ | @apply-error@ | @unknown-error@
awsRedshiftClusterClusterParameterStatus_parameterApplyStatus :: Lens.Lens' AwsRedshiftClusterClusterParameterStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterParameterStatus_parameterApplyStatus = Lens.lens (\AwsRedshiftClusterClusterParameterStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@AwsRedshiftClusterClusterParameterStatus' {} a -> s {parameterApplyStatus = a} :: AwsRedshiftClusterClusterParameterStatus)

-- | The name of the parameter.
awsRedshiftClusterClusterParameterStatus_parameterName :: Lens.Lens' AwsRedshiftClusterClusterParameterStatus (Prelude.Maybe Prelude.Text)
awsRedshiftClusterClusterParameterStatus_parameterName = Lens.lens (\AwsRedshiftClusterClusterParameterStatus' {parameterName} -> parameterName) (\s@AwsRedshiftClusterClusterParameterStatus' {} a -> s {parameterName = a} :: AwsRedshiftClusterClusterParameterStatus)

instance
  Data.FromJSON
    AwsRedshiftClusterClusterParameterStatus
  where
  parseJSON =
    Data.withObject
      "AwsRedshiftClusterClusterParameterStatus"
      ( \x ->
          AwsRedshiftClusterClusterParameterStatus'
            Prelude.<$> (x Data..:? "ParameterApplyErrorDescription")
            Prelude.<*> (x Data..:? "ParameterApplyStatus")
            Prelude.<*> (x Data..:? "ParameterName")
      )

instance
  Prelude.Hashable
    AwsRedshiftClusterClusterParameterStatus
  where
  hashWithSalt
    _salt
    AwsRedshiftClusterClusterParameterStatus' {..} =
      _salt
        `Prelude.hashWithSalt` parameterApplyErrorDescription
        `Prelude.hashWithSalt` parameterApplyStatus
        `Prelude.hashWithSalt` parameterName

instance
  Prelude.NFData
    AwsRedshiftClusterClusterParameterStatus
  where
  rnf AwsRedshiftClusterClusterParameterStatus' {..} =
    Prelude.rnf parameterApplyErrorDescription
      `Prelude.seq` Prelude.rnf parameterApplyStatus
      `Prelude.seq` Prelude.rnf parameterName

instance
  Data.ToJSON
    AwsRedshiftClusterClusterParameterStatus
  where
  toJSON AwsRedshiftClusterClusterParameterStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParameterApplyErrorDescription" Data..=)
              Prelude.<$> parameterApplyErrorDescription,
            ("ParameterApplyStatus" Data..=)
              Prelude.<$> parameterApplyStatus,
            ("ParameterName" Data..=) Prelude.<$> parameterName
          ]
      )
