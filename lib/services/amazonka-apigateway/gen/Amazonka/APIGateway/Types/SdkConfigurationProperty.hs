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
-- Module      : Amazonka.APIGateway.Types.SdkConfigurationProperty
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.SdkConfigurationProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A configuration property of an SDK type.
--
-- /See:/ 'newSdkConfigurationProperty' smart constructor.
data SdkConfigurationProperty = SdkConfigurationProperty'
  { -- | The default value of an SdkType configuration property.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The description of an SdkType configuration property.
    description :: Prelude.Maybe Prelude.Text,
    -- | The user-friendly name of an SdkType configuration property.
    friendlyName :: Prelude.Maybe Prelude.Text,
    -- | The name of a an SdkType configuration property.
    name :: Prelude.Maybe Prelude.Text,
    -- | A boolean flag of an SdkType configuration property to indicate if the
    -- associated SDK configuration property is required (@true@) or not
    -- (@false@).
    required :: Prelude.Maybe Prelude.Bool
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
-- 'defaultValue', 'sdkConfigurationProperty_defaultValue' - The default value of an SdkType configuration property.
--
-- 'description', 'sdkConfigurationProperty_description' - The description of an SdkType configuration property.
--
-- 'friendlyName', 'sdkConfigurationProperty_friendlyName' - The user-friendly name of an SdkType configuration property.
--
-- 'name', 'sdkConfigurationProperty_name' - The name of a an SdkType configuration property.
--
-- 'required', 'sdkConfigurationProperty_required' - A boolean flag of an SdkType configuration property to indicate if the
-- associated SDK configuration property is required (@true@) or not
-- (@false@).
newSdkConfigurationProperty ::
  SdkConfigurationProperty
newSdkConfigurationProperty =
  SdkConfigurationProperty'
    { defaultValue =
        Prelude.Nothing,
      description = Prelude.Nothing,
      friendlyName = Prelude.Nothing,
      name = Prelude.Nothing,
      required = Prelude.Nothing
    }

-- | The default value of an SdkType configuration property.
sdkConfigurationProperty_defaultValue :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_defaultValue = Lens.lens (\SdkConfigurationProperty' {defaultValue} -> defaultValue) (\s@SdkConfigurationProperty' {} a -> s {defaultValue = a} :: SdkConfigurationProperty)

-- | The description of an SdkType configuration property.
sdkConfigurationProperty_description :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_description = Lens.lens (\SdkConfigurationProperty' {description} -> description) (\s@SdkConfigurationProperty' {} a -> s {description = a} :: SdkConfigurationProperty)

-- | The user-friendly name of an SdkType configuration property.
sdkConfigurationProperty_friendlyName :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_friendlyName = Lens.lens (\SdkConfigurationProperty' {friendlyName} -> friendlyName) (\s@SdkConfigurationProperty' {} a -> s {friendlyName = a} :: SdkConfigurationProperty)

-- | The name of a an SdkType configuration property.
sdkConfigurationProperty_name :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Text)
sdkConfigurationProperty_name = Lens.lens (\SdkConfigurationProperty' {name} -> name) (\s@SdkConfigurationProperty' {} a -> s {name = a} :: SdkConfigurationProperty)

-- | A boolean flag of an SdkType configuration property to indicate if the
-- associated SDK configuration property is required (@true@) or not
-- (@false@).
sdkConfigurationProperty_required :: Lens.Lens' SdkConfigurationProperty (Prelude.Maybe Prelude.Bool)
sdkConfigurationProperty_required = Lens.lens (\SdkConfigurationProperty' {required} -> required) (\s@SdkConfigurationProperty' {} a -> s {required = a} :: SdkConfigurationProperty)

instance Data.FromJSON SdkConfigurationProperty where
  parseJSON =
    Data.withObject
      "SdkConfigurationProperty"
      ( \x ->
          SdkConfigurationProperty'
            Prelude.<$> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "friendlyName")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "required")
      )

instance Prelude.Hashable SdkConfigurationProperty where
  hashWithSalt _salt SdkConfigurationProperty' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` friendlyName
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` required

instance Prelude.NFData SdkConfigurationProperty where
  rnf SdkConfigurationProperty' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf friendlyName
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf required
