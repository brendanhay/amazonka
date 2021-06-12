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
-- Module      : Network.AWS.APIGateway.Types.SdkConfigurationProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.SdkConfigurationProperty where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A configuration property of an SDK type.
--
-- /See:/ 'newSdkConfigurationProperty' smart constructor.
data SdkConfigurationProperty = SdkConfigurationProperty'
  { -- | A boolean flag of an SdkType configuration property to indicate if the
    -- associated SDK configuration property is required (@true@) or not
    -- (@false@).
    required :: Core.Maybe Core.Bool,
    -- | The user-friendly name of an SdkType configuration property.
    friendlyName :: Core.Maybe Core.Text,
    -- | The name of a an SdkType configuration property.
    name :: Core.Maybe Core.Text,
    -- | The description of an SdkType configuration property.
    description :: Core.Maybe Core.Text,
    -- | The default value of an SdkType configuration property.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SdkConfigurationProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'required', 'sdkConfigurationProperty_required' - A boolean flag of an SdkType configuration property to indicate if the
-- associated SDK configuration property is required (@true@) or not
-- (@false@).
--
-- 'friendlyName', 'sdkConfigurationProperty_friendlyName' - The user-friendly name of an SdkType configuration property.
--
-- 'name', 'sdkConfigurationProperty_name' - The name of a an SdkType configuration property.
--
-- 'description', 'sdkConfigurationProperty_description' - The description of an SdkType configuration property.
--
-- 'defaultValue', 'sdkConfigurationProperty_defaultValue' - The default value of an SdkType configuration property.
newSdkConfigurationProperty ::
  SdkConfigurationProperty
newSdkConfigurationProperty =
  SdkConfigurationProperty'
    { required = Core.Nothing,
      friendlyName = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | A boolean flag of an SdkType configuration property to indicate if the
-- associated SDK configuration property is required (@true@) or not
-- (@false@).
sdkConfigurationProperty_required :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Bool)
sdkConfigurationProperty_required = Lens.lens (\SdkConfigurationProperty' {required} -> required) (\s@SdkConfigurationProperty' {} a -> s {required = a} :: SdkConfigurationProperty)

-- | The user-friendly name of an SdkType configuration property.
sdkConfigurationProperty_friendlyName :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
sdkConfigurationProperty_friendlyName = Lens.lens (\SdkConfigurationProperty' {friendlyName} -> friendlyName) (\s@SdkConfigurationProperty' {} a -> s {friendlyName = a} :: SdkConfigurationProperty)

-- | The name of a an SdkType configuration property.
sdkConfigurationProperty_name :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
sdkConfigurationProperty_name = Lens.lens (\SdkConfigurationProperty' {name} -> name) (\s@SdkConfigurationProperty' {} a -> s {name = a} :: SdkConfigurationProperty)

-- | The description of an SdkType configuration property.
sdkConfigurationProperty_description :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
sdkConfigurationProperty_description = Lens.lens (\SdkConfigurationProperty' {description} -> description) (\s@SdkConfigurationProperty' {} a -> s {description = a} :: SdkConfigurationProperty)

-- | The default value of an SdkType configuration property.
sdkConfigurationProperty_defaultValue :: Lens.Lens' SdkConfigurationProperty (Core.Maybe Core.Text)
sdkConfigurationProperty_defaultValue = Lens.lens (\SdkConfigurationProperty' {defaultValue} -> defaultValue) (\s@SdkConfigurationProperty' {} a -> s {defaultValue = a} :: SdkConfigurationProperty)

instance Core.FromJSON SdkConfigurationProperty where
  parseJSON =
    Core.withObject
      "SdkConfigurationProperty"
      ( \x ->
          SdkConfigurationProperty'
            Core.<$> (x Core..:? "required")
            Core.<*> (x Core..:? "friendlyName")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "defaultValue")
      )

instance Core.Hashable SdkConfigurationProperty

instance Core.NFData SdkConfigurationProperty
