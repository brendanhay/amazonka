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
-- Module      : Network.AWS.Redshift.Types.ClusterParameterStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | Describes the status of a parameter group.
--
-- /See:/ 'newClusterParameterStatus' smart constructor.
data ClusterParameterStatus = ClusterParameterStatus'
  { -- | The status of the parameter that indicates whether the parameter is in
    -- sync with the database, waiting for a cluster reboot, or encountered an
    -- error when being applied.
    --
    -- The following are possible statuses and descriptions.
    --
    -- -   @in-sync@: The parameter value is in sync with the database.
    --
    -- -   @pending-reboot@: The parameter value will be applied after the
    --     cluster reboots.
    --
    -- -   @applying@: The parameter value is being applied to the database.
    --
    -- -   @invalid-parameter@: Cannot apply the parameter value because it has
    --     an invalid value or syntax.
    --
    -- -   @apply-deferred@: The parameter contains static property changes.
    --     The changes are deferred until the cluster reboots.
    --
    -- -   @apply-error@: Cannot connect to the cluster. The parameter change
    --     will be applied after the cluster reboots.
    --
    -- -   @unknown-error@: Cannot apply the parameter change right now. The
    --     change will be applied after the cluster reboots.
    parameterApplyStatus :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | The error that prevented the parameter from being applied to the
    -- database.
    parameterApplyErrorDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClusterParameterStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterApplyStatus', 'clusterParameterStatus_parameterApplyStatus' - The status of the parameter that indicates whether the parameter is in
-- sync with the database, waiting for a cluster reboot, or encountered an
-- error when being applied.
--
-- The following are possible statuses and descriptions.
--
-- -   @in-sync@: The parameter value is in sync with the database.
--
-- -   @pending-reboot@: The parameter value will be applied after the
--     cluster reboots.
--
-- -   @applying@: The parameter value is being applied to the database.
--
-- -   @invalid-parameter@: Cannot apply the parameter value because it has
--     an invalid value or syntax.
--
-- -   @apply-deferred@: The parameter contains static property changes.
--     The changes are deferred until the cluster reboots.
--
-- -   @apply-error@: Cannot connect to the cluster. The parameter change
--     will be applied after the cluster reboots.
--
-- -   @unknown-error@: Cannot apply the parameter change right now. The
--     change will be applied after the cluster reboots.
--
-- 'parameterName', 'clusterParameterStatus_parameterName' - The name of the parameter.
--
-- 'parameterApplyErrorDescription', 'clusterParameterStatus_parameterApplyErrorDescription' - The error that prevented the parameter from being applied to the
-- database.
newClusterParameterStatus ::
  ClusterParameterStatus
newClusterParameterStatus =
  ClusterParameterStatus'
    { parameterApplyStatus =
        Prelude.Nothing,
      parameterName = Prelude.Nothing,
      parameterApplyErrorDescription = Prelude.Nothing
    }

-- | The status of the parameter that indicates whether the parameter is in
-- sync with the database, waiting for a cluster reboot, or encountered an
-- error when being applied.
--
-- The following are possible statuses and descriptions.
--
-- -   @in-sync@: The parameter value is in sync with the database.
--
-- -   @pending-reboot@: The parameter value will be applied after the
--     cluster reboots.
--
-- -   @applying@: The parameter value is being applied to the database.
--
-- -   @invalid-parameter@: Cannot apply the parameter value because it has
--     an invalid value or syntax.
--
-- -   @apply-deferred@: The parameter contains static property changes.
--     The changes are deferred until the cluster reboots.
--
-- -   @apply-error@: Cannot connect to the cluster. The parameter change
--     will be applied after the cluster reboots.
--
-- -   @unknown-error@: Cannot apply the parameter change right now. The
--     change will be applied after the cluster reboots.
clusterParameterStatus_parameterApplyStatus :: Lens.Lens' ClusterParameterStatus (Prelude.Maybe Prelude.Text)
clusterParameterStatus_parameterApplyStatus = Lens.lens (\ClusterParameterStatus' {parameterApplyStatus} -> parameterApplyStatus) (\s@ClusterParameterStatus' {} a -> s {parameterApplyStatus = a} :: ClusterParameterStatus)

-- | The name of the parameter.
clusterParameterStatus_parameterName :: Lens.Lens' ClusterParameterStatus (Prelude.Maybe Prelude.Text)
clusterParameterStatus_parameterName = Lens.lens (\ClusterParameterStatus' {parameterName} -> parameterName) (\s@ClusterParameterStatus' {} a -> s {parameterName = a} :: ClusterParameterStatus)

-- | The error that prevented the parameter from being applied to the
-- database.
clusterParameterStatus_parameterApplyErrorDescription :: Lens.Lens' ClusterParameterStatus (Prelude.Maybe Prelude.Text)
clusterParameterStatus_parameterApplyErrorDescription = Lens.lens (\ClusterParameterStatus' {parameterApplyErrorDescription} -> parameterApplyErrorDescription) (\s@ClusterParameterStatus' {} a -> s {parameterApplyErrorDescription = a} :: ClusterParameterStatus)

instance Prelude.FromXML ClusterParameterStatus where
  parseXML x =
    ClusterParameterStatus'
      Prelude.<$> (x Prelude..@? "ParameterApplyStatus")
      Prelude.<*> (x Prelude..@? "ParameterName")
      Prelude.<*> (x Prelude..@? "ParameterApplyErrorDescription")

instance Prelude.Hashable ClusterParameterStatus

instance Prelude.NFData ClusterParameterStatus
