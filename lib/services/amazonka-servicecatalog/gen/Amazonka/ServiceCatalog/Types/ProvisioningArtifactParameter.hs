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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ParameterConstraints

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'newProvisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { -- | If this value is true, the value for this parameter is obfuscated from
    -- view when the parameter is retrieved. This parameter is used to hide
    -- sensitive information.
    isNoEcho :: Prelude.Maybe Prelude.Bool,
    -- | The parameter key.
    parameterKey :: Prelude.Maybe Prelude.Text,
    -- | The parameter type.
    parameterType :: Prelude.Maybe Prelude.Text,
    -- | Constraints that the administrator has put on a parameter.
    parameterConstraints :: Prelude.Maybe ParameterConstraints,
    -- | The default value.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The description of the parameter.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'parameterKey', 'provisioningArtifactParameter_parameterKey' - The parameter key.
--
-- 'parameterType', 'provisioningArtifactParameter_parameterType' - The parameter type.
--
-- 'parameterConstraints', 'provisioningArtifactParameter_parameterConstraints' - Constraints that the administrator has put on a parameter.
--
-- 'defaultValue', 'provisioningArtifactParameter_defaultValue' - The default value.
--
-- 'description', 'provisioningArtifactParameter_description' - The description of the parameter.
newProvisioningArtifactParameter ::
  ProvisioningArtifactParameter
newProvisioningArtifactParameter =
  ProvisioningArtifactParameter'
    { isNoEcho =
        Prelude.Nothing,
      parameterKey = Prelude.Nothing,
      parameterType = Prelude.Nothing,
      parameterConstraints = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | If this value is true, the value for this parameter is obfuscated from
-- view when the parameter is retrieved. This parameter is used to hide
-- sensitive information.
provisioningArtifactParameter_isNoEcho :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Bool)
provisioningArtifactParameter_isNoEcho = Lens.lens (\ProvisioningArtifactParameter' {isNoEcho} -> isNoEcho) (\s@ProvisioningArtifactParameter' {} a -> s {isNoEcho = a} :: ProvisioningArtifactParameter)

-- | The parameter key.
provisioningArtifactParameter_parameterKey :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_parameterKey = Lens.lens (\ProvisioningArtifactParameter' {parameterKey} -> parameterKey) (\s@ProvisioningArtifactParameter' {} a -> s {parameterKey = a} :: ProvisioningArtifactParameter)

-- | The parameter type.
provisioningArtifactParameter_parameterType :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_parameterType = Lens.lens (\ProvisioningArtifactParameter' {parameterType} -> parameterType) (\s@ProvisioningArtifactParameter' {} a -> s {parameterType = a} :: ProvisioningArtifactParameter)

-- | Constraints that the administrator has put on a parameter.
provisioningArtifactParameter_parameterConstraints :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe ParameterConstraints)
provisioningArtifactParameter_parameterConstraints = Lens.lens (\ProvisioningArtifactParameter' {parameterConstraints} -> parameterConstraints) (\s@ProvisioningArtifactParameter' {} a -> s {parameterConstraints = a} :: ProvisioningArtifactParameter)

-- | The default value.
provisioningArtifactParameter_defaultValue :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_defaultValue = Lens.lens (\ProvisioningArtifactParameter' {defaultValue} -> defaultValue) (\s@ProvisioningArtifactParameter' {} a -> s {defaultValue = a} :: ProvisioningArtifactParameter)

-- | The description of the parameter.
provisioningArtifactParameter_description :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_description = Lens.lens (\ProvisioningArtifactParameter' {description} -> description) (\s@ProvisioningArtifactParameter' {} a -> s {description = a} :: ProvisioningArtifactParameter)

instance Core.FromJSON ProvisioningArtifactParameter where
  parseJSON =
    Core.withObject
      "ProvisioningArtifactParameter"
      ( \x ->
          ProvisioningArtifactParameter'
            Prelude.<$> (x Core..:? "IsNoEcho")
            Prelude.<*> (x Core..:? "ParameterKey")
            Prelude.<*> (x Core..:? "ParameterType")
            Prelude.<*> (x Core..:? "ParameterConstraints")
            Prelude.<*> (x Core..:? "DefaultValue")
            Prelude.<*> (x Core..:? "Description")
      )

instance
  Prelude.Hashable
    ProvisioningArtifactParameter

instance Prelude.NFData ProvisioningArtifactParameter
