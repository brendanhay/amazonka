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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.ParameterConstraints

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'newProvisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { -- | If this value is true, the value for this parameter is obfuscated from
    -- view when the parameter is retrieved. This parameter is used to hide
    -- sensitive information.
    isNoEcho :: Core.Maybe Core.Bool,
    -- | Constraints that the administrator has put on a parameter.
    parameterConstraints :: Core.Maybe ParameterConstraints,
    -- | The parameter type.
    parameterType :: Core.Maybe Core.Text,
    -- | The parameter key.
    parameterKey :: Core.Maybe Core.Text,
    -- | The description of the parameter.
    description :: Core.Maybe Core.Text,
    -- | The default value.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisioningArtifactParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isNoEcho', 'provisioningArtifactParameter_isNoEcho' - If this value is true, the value for this parameter is obfuscated from
-- view when the parameter is retrieved. This parameter is used to hide
-- sensitive information.
--
-- 'parameterConstraints', 'provisioningArtifactParameter_parameterConstraints' - Constraints that the administrator has put on a parameter.
--
-- 'parameterType', 'provisioningArtifactParameter_parameterType' - The parameter type.
--
-- 'parameterKey', 'provisioningArtifactParameter_parameterKey' - The parameter key.
--
-- 'description', 'provisioningArtifactParameter_description' - The description of the parameter.
--
-- 'defaultValue', 'provisioningArtifactParameter_defaultValue' - The default value.
newProvisioningArtifactParameter ::
  ProvisioningArtifactParameter
newProvisioningArtifactParameter =
  ProvisioningArtifactParameter'
    { isNoEcho =
        Core.Nothing,
      parameterConstraints = Core.Nothing,
      parameterType = Core.Nothing,
      parameterKey = Core.Nothing,
      description = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | If this value is true, the value for this parameter is obfuscated from
-- view when the parameter is retrieved. This parameter is used to hide
-- sensitive information.
provisioningArtifactParameter_isNoEcho :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Core.Bool)
provisioningArtifactParameter_isNoEcho = Lens.lens (\ProvisioningArtifactParameter' {isNoEcho} -> isNoEcho) (\s@ProvisioningArtifactParameter' {} a -> s {isNoEcho = a} :: ProvisioningArtifactParameter)

-- | Constraints that the administrator has put on a parameter.
provisioningArtifactParameter_parameterConstraints :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe ParameterConstraints)
provisioningArtifactParameter_parameterConstraints = Lens.lens (\ProvisioningArtifactParameter' {parameterConstraints} -> parameterConstraints) (\s@ProvisioningArtifactParameter' {} a -> s {parameterConstraints = a} :: ProvisioningArtifactParameter)

-- | The parameter type.
provisioningArtifactParameter_parameterType :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Core.Text)
provisioningArtifactParameter_parameterType = Lens.lens (\ProvisioningArtifactParameter' {parameterType} -> parameterType) (\s@ProvisioningArtifactParameter' {} a -> s {parameterType = a} :: ProvisioningArtifactParameter)

-- | The parameter key.
provisioningArtifactParameter_parameterKey :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Core.Text)
provisioningArtifactParameter_parameterKey = Lens.lens (\ProvisioningArtifactParameter' {parameterKey} -> parameterKey) (\s@ProvisioningArtifactParameter' {} a -> s {parameterKey = a} :: ProvisioningArtifactParameter)

-- | The description of the parameter.
provisioningArtifactParameter_description :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Core.Text)
provisioningArtifactParameter_description = Lens.lens (\ProvisioningArtifactParameter' {description} -> description) (\s@ProvisioningArtifactParameter' {} a -> s {description = a} :: ProvisioningArtifactParameter)

-- | The default value.
provisioningArtifactParameter_defaultValue :: Lens.Lens' ProvisioningArtifactParameter (Core.Maybe Core.Text)
provisioningArtifactParameter_defaultValue = Lens.lens (\ProvisioningArtifactParameter' {defaultValue} -> defaultValue) (\s@ProvisioningArtifactParameter' {} a -> s {defaultValue = a} :: ProvisioningArtifactParameter)

instance Core.FromJSON ProvisioningArtifactParameter where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactParameter"
      ( \x ->
          ProvisioningArtifactParameter'
            Core.<$> (x Core..:? "IsNoEcho")
            Core.<*> (x Core..:? "ParameterConstraints")
            Core.<*> (x Core..:? "ParameterType")
            Core.<*> (x Core..:? "ParameterKey")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "DefaultValue")
      )

instance Core.Hashable ProvisioningArtifactParameter

instance Core.NFData ProvisioningArtifactParameter
