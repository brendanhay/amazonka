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
-- Module      : Network.AWS.ElastiCache.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Parameter where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.ChangeType
import qualified Network.AWS.Lens as Lens

-- | Describes an individual setting that controls some aspect of ElastiCache
-- behavior.
--
-- /See:/ 'newParameter' smart constructor.
data Parameter = Parameter'
  { -- | Indicates whether a change to the parameter is applied immediately or
    -- requires a reboot for the change to be applied. You can force a reboot
    -- or wait until the next maintenance window\'s reboot. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
    changeType :: Core.Maybe ChangeType,
    -- | The valid range of values for the parameter.
    allowedValues :: Core.Maybe Core.Text,
    -- | The source of the parameter.
    source :: Core.Maybe Core.Text,
    -- | The value of the parameter.
    parameterValue :: Core.Maybe Core.Text,
    -- | The name of the parameter.
    parameterName :: Core.Maybe Core.Text,
    -- | A description of the parameter.
    description :: Core.Maybe Core.Text,
    -- | The valid data type for the parameter.
    dataType :: Core.Maybe Core.Text,
    -- | Indicates whether (@true@) or not (@false@) the parameter can be
    -- modified. Some parameters have security or operational implications that
    -- prevent them from being changed.
    isModifiable :: Core.Maybe Core.Bool,
    -- | The earliest cache engine version to which the parameter can apply.
    minimumEngineVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Parameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeType', 'parameter_changeType' - Indicates whether a change to the parameter is applied immediately or
-- requires a reboot for the change to be applied. You can force a reboot
-- or wait until the next maintenance window\'s reboot. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
--
-- 'allowedValues', 'parameter_allowedValues' - The valid range of values for the parameter.
--
-- 'source', 'parameter_source' - The source of the parameter.
--
-- 'parameterValue', 'parameter_parameterValue' - The value of the parameter.
--
-- 'parameterName', 'parameter_parameterName' - The name of the parameter.
--
-- 'description', 'parameter_description' - A description of the parameter.
--
-- 'dataType', 'parameter_dataType' - The valid data type for the parameter.
--
-- 'isModifiable', 'parameter_isModifiable' - Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
--
-- 'minimumEngineVersion', 'parameter_minimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
newParameter ::
  Parameter
newParameter =
  Parameter'
    { changeType = Core.Nothing,
      allowedValues = Core.Nothing,
      source = Core.Nothing,
      parameterValue = Core.Nothing,
      parameterName = Core.Nothing,
      description = Core.Nothing,
      dataType = Core.Nothing,
      isModifiable = Core.Nothing,
      minimumEngineVersion = Core.Nothing
    }

-- | Indicates whether a change to the parameter is applied immediately or
-- requires a reboot for the change to be applied. You can force a reboot
-- or wait until the next maintenance window\'s reboot. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
parameter_changeType :: Lens.Lens' Parameter (Core.Maybe ChangeType)
parameter_changeType = Lens.lens (\Parameter' {changeType} -> changeType) (\s@Parameter' {} a -> s {changeType = a} :: Parameter)

-- | The valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The source of the parameter.
parameter_source :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | The value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A description of the parameter.
parameter_description :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The valid data type for the parameter.
parameter_dataType :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Core.Maybe Core.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The earliest cache engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Core.Maybe Core.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

instance Core.FromXML Parameter where
  parseXML x =
    Parameter'
      Core.<$> (x Core..@? "ChangeType")
      Core.<*> (x Core..@? "AllowedValues")
      Core.<*> (x Core..@? "Source")
      Core.<*> (x Core..@? "ParameterValue")
      Core.<*> (x Core..@? "ParameterName")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "DataType")
      Core.<*> (x Core..@? "IsModifiable")
      Core.<*> (x Core..@? "MinimumEngineVersion")

instance Core.Hashable Parameter

instance Core.NFData Parameter
