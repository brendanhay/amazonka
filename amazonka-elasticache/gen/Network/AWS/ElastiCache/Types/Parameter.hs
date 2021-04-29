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
-- Module      : Network.AWS.ElastiCache.Types.Parameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Parameter where

import Network.AWS.ElastiCache.Types.ChangeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    changeType :: Prelude.Maybe ChangeType,
    -- | The valid range of values for the parameter.
    allowedValues :: Prelude.Maybe Prelude.Text,
    -- | The source of the parameter.
    source :: Prelude.Maybe Prelude.Text,
    -- | The value of the parameter.
    parameterValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    parameterName :: Prelude.Maybe Prelude.Text,
    -- | A description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The valid data type for the parameter.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether (@true@) or not (@false@) the parameter can be
    -- modified. Some parameters have security or operational implications that
    -- prevent them from being changed.
    isModifiable :: Prelude.Maybe Prelude.Bool,
    -- | The earliest cache engine version to which the parameter can apply.
    minimumEngineVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { changeType = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      source = Prelude.Nothing,
      parameterValue = Prelude.Nothing,
      parameterName = Prelude.Nothing,
      description = Prelude.Nothing,
      dataType = Prelude.Nothing,
      isModifiable = Prelude.Nothing,
      minimumEngineVersion = Prelude.Nothing
    }

-- | Indicates whether a change to the parameter is applied immediately or
-- requires a reboot for the change to be applied. You can force a reboot
-- or wait until the next maintenance window\'s reboot. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.Rebooting.html Rebooting a Cluster>.
parameter_changeType :: Lens.Lens' Parameter (Prelude.Maybe ChangeType)
parameter_changeType = Lens.lens (\Parameter' {changeType} -> changeType) (\s@Parameter' {} a -> s {changeType = a} :: Parameter)

-- | The valid range of values for the parameter.
parameter_allowedValues :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_allowedValues = Lens.lens (\Parameter' {allowedValues} -> allowedValues) (\s@Parameter' {} a -> s {allowedValues = a} :: Parameter)

-- | The source of the parameter.
parameter_source :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_source = Lens.lens (\Parameter' {source} -> source) (\s@Parameter' {} a -> s {source = a} :: Parameter)

-- | The value of the parameter.
parameter_parameterValue :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterValue = Lens.lens (\Parameter' {parameterValue} -> parameterValue) (\s@Parameter' {} a -> s {parameterValue = a} :: Parameter)

-- | The name of the parameter.
parameter_parameterName :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_parameterName = Lens.lens (\Parameter' {parameterName} -> parameterName) (\s@Parameter' {} a -> s {parameterName = a} :: Parameter)

-- | A description of the parameter.
parameter_description :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_description = Lens.lens (\Parameter' {description} -> description) (\s@Parameter' {} a -> s {description = a} :: Parameter)

-- | The valid data type for the parameter.
parameter_dataType :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_dataType = Lens.lens (\Parameter' {dataType} -> dataType) (\s@Parameter' {} a -> s {dataType = a} :: Parameter)

-- | Indicates whether (@true@) or not (@false@) the parameter can be
-- modified. Some parameters have security or operational implications that
-- prevent them from being changed.
parameter_isModifiable :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Bool)
parameter_isModifiable = Lens.lens (\Parameter' {isModifiable} -> isModifiable) (\s@Parameter' {} a -> s {isModifiable = a} :: Parameter)

-- | The earliest cache engine version to which the parameter can apply.
parameter_minimumEngineVersion :: Lens.Lens' Parameter (Prelude.Maybe Prelude.Text)
parameter_minimumEngineVersion = Lens.lens (\Parameter' {minimumEngineVersion} -> minimumEngineVersion) (\s@Parameter' {} a -> s {minimumEngineVersion = a} :: Parameter)

instance Prelude.FromXML Parameter where
  parseXML x =
    Parameter'
      Prelude.<$> (x Prelude..@? "ChangeType")
      Prelude.<*> (x Prelude..@? "AllowedValues")
      Prelude.<*> (x Prelude..@? "Source")
      Prelude.<*> (x Prelude..@? "ParameterValue")
      Prelude.<*> (x Prelude..@? "ParameterName")
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "DataType")
      Prelude.<*> (x Prelude..@? "IsModifiable")
      Prelude.<*> (x Prelude..@? "MinimumEngineVersion")

instance Prelude.Hashable Parameter

instance Prelude.NFData Parameter
