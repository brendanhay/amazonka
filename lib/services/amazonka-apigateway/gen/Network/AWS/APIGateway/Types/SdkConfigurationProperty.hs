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
import qualified Network.AWS.Prelude as Prelude

-- | A configuration property of an SDK type.
--
-- /See:/ 'newSdkConfigurationProperty' smart constructor.
data SdkConfigurationProperty = SdkConfigurationProperty'
  { -- | The user-friendly name of an SdkType configuration property.
    friendlyName :: Prelude.Maybe Prelude.Text,
    -- | A boolean flag of an SdkType configuration property to indicate if the
    -- associated SDK configuration property is required (@true@) or not
    -- (@false@).
    required :: Prelude.Maybe Prelude.Bool,
    -- | The name of a an SdkType configuration property.
    name :: Prelude.Maybe Prelude.Text,
    -- | The default value of an SdkType configuration property.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The description of an SdkType configuration property.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SdkConfigurationProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'friendlyName', 'sdkConfigurationProperty_friendlyName' - The user-friendly name of an SdkType configuration property.
--
-- 'required', 'sdkConfigurationProperty_required' - A boolean flag of an SdkType configuration property to indicate if the
-- associated SDK configuration property is required (@true@) or not
-- (@false@).
--
-- 'name', 'sdkConfigurationProperty_name' - The name of a an SdkType configuration property.
--
-- 'defaultValue', 'sdkConfigurationProperty_defaultValue' - The default value of an SdkType configuration property.
--
-- 'description', 'sdkConfigurationProperty_description' - The description of an SdkType configuration property.
newSdkConfigurationProperty ::
  SdkConfigurationProperty
newSdkConfigurationProperty =
  SdkConfigurationProperty'
    { friendlyName =
        Prelude.Nothing,
      required = Prelude.Nothing,
      name = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The user-friendly name of an SdkType configuration property.
sdkConfigurationProperty_friendlyName :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_friendlyName = Lens.lens (\SdkConfigurationProperty' {friendlyName} -> friendlyName) (\s@SdkConfigurationProperty' {} a -> s {friendlyName = a} :: SdkConfigurationProperty)

-- | A boolean flag of an SdkType configuration property to indicate if the
-- associated SDK configuration property is required (@true@) or not
-- (@false@).
sdkConfigurationProperty_required :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Bool)
sdkConfigurationProperty_required = Lens.lens (\SdkConfigurationProperty' {required} -> required) (\s@SdkConfigurationProperty' {} a -> s {required = a} :: SdkConfigurationProperty)

-- | The name of a an SdkType configuration property.
sdkConfigurationProperty_name :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_name = Lens.lens (\SdkConfigurationProperty' {name} -> name) (\s@SdkConfigurationProperty' {} a -> s {name = a} :: SdkConfigurationProperty)

-- | The default value of an SdkType configuration property.
sdkConfigurationProperty_defaultValue :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_defaultValue = Lens.lens (\SdkConfigurationProperty' {defaultValue} -> defaultValue) (\s@SdkConfigurationProperty' {} a -> s {defaultValue = a} :: SdkConfigurationProperty)

-- | The description of an SdkType configuration property.
sdkConfigurationProperty_description :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_description = Lens.lens (\SdkConfigurationProperty' {description} -> description) (\s@SdkConfigurationProperty' {} a -> s {description = a} :: SdkConfigurationProperty)

instance Core.FromJSON SdkConfigurationProperty where
  parseJSON =
    Core.withObject
      "SdkConfigurationProperty"
      ( \x ->
          SdkConfigurationProperty'
            Prelude.<$> (x Core..:? "friendlyName")
            Prelude.<*> (x Core..:? "required")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "defaultValue")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable SdkConfigurationProperty

instance Prelude.NFData SdkConfigurationProperty
