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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactParameter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ParameterConstraints

-- | Information about a parameter used to provision a product.
--
-- /See:/ 'newProvisioningArtifactParameter' smart constructor.
data ProvisioningArtifactParameter = ProvisioningArtifactParameter'
  { -- | If this value is true, the value for this parameter is obfuscated from
    -- view when the parameter is retrieved. This parameter is used to hide
    -- sensitive information.
    isNoEcho :: Prelude.Maybe Prelude.Bool,
    -- | Constraints that the administrator has put on a parameter.
    parameterConstraints :: Prelude.Maybe ParameterConstraints,
    -- | The parameter type.
    parameterType :: Prelude.Maybe Prelude.Text,
    -- | The parameter key.
    parameterKey :: Prelude.Maybe Prelude.Text,
    -- | The description of the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The default value.
    defaultValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      parameterConstraints = Prelude.Nothing,
      parameterType = Prelude.Nothing,
      parameterKey = Prelude.Nothing,
      description = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | If this value is true, the value for this parameter is obfuscated from
-- view when the parameter is retrieved. This parameter is used to hide
-- sensitive information.
provisioningArtifactParameter_isNoEcho :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Bool)
provisioningArtifactParameter_isNoEcho = Lens.lens (\ProvisioningArtifactParameter' {isNoEcho} -> isNoEcho) (\s@ProvisioningArtifactParameter' {} a -> s {isNoEcho = a} :: ProvisioningArtifactParameter)

-- | Constraints that the administrator has put on a parameter.
provisioningArtifactParameter_parameterConstraints :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe ParameterConstraints)
provisioningArtifactParameter_parameterConstraints = Lens.lens (\ProvisioningArtifactParameter' {parameterConstraints} -> parameterConstraints) (\s@ProvisioningArtifactParameter' {} a -> s {parameterConstraints = a} :: ProvisioningArtifactParameter)

-- | The parameter type.
provisioningArtifactParameter_parameterType :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_parameterType = Lens.lens (\ProvisioningArtifactParameter' {parameterType} -> parameterType) (\s@ProvisioningArtifactParameter' {} a -> s {parameterType = a} :: ProvisioningArtifactParameter)

-- | The parameter key.
provisioningArtifactParameter_parameterKey :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_parameterKey = Lens.lens (\ProvisioningArtifactParameter' {parameterKey} -> parameterKey) (\s@ProvisioningArtifactParameter' {} a -> s {parameterKey = a} :: ProvisioningArtifactParameter)

-- | The description of the parameter.
provisioningArtifactParameter_description :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_description = Lens.lens (\ProvisioningArtifactParameter' {description} -> description) (\s@ProvisioningArtifactParameter' {} a -> s {description = a} :: ProvisioningArtifactParameter)

-- | The default value.
provisioningArtifactParameter_defaultValue :: Lens.Lens' ProvisioningArtifactParameter (Prelude.Maybe Prelude.Text)
provisioningArtifactParameter_defaultValue = Lens.lens (\ProvisioningArtifactParameter' {defaultValue} -> defaultValue) (\s@ProvisioningArtifactParameter' {} a -> s {defaultValue = a} :: ProvisioningArtifactParameter)

instance
  Prelude.FromJSON
    ProvisioningArtifactParameter
  where
  parseJSON =
    Prelude.withObject
      "ProvisioningArtifactParameter"
      ( \x ->
          ProvisioningArtifactParameter'
            Prelude.<$> (x Prelude..:? "IsNoEcho")
            Prelude.<*> (x Prelude..:? "ParameterConstraints")
            Prelude.<*> (x Prelude..:? "ParameterType")
            Prelude.<*> (x Prelude..:? "ParameterKey")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "DefaultValue")
      )

instance
  Prelude.Hashable
    ProvisioningArtifactParameter

instance Prelude.NFData ProvisioningArtifactParameter
