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
-- Module      : Amazonka.AppFlow.Types.CustomConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.CustomConnectorProfileProperties where

import Amazonka.AppFlow.Types.OAuth2Properties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The profile properties required by the custom connector.
--
-- /See:/ 'newCustomConnectorProfileProperties' smart constructor.
data CustomConnectorProfileProperties = CustomConnectorProfileProperties'
  { -- | A map of properties that are required to create a profile for the custom
    -- connector.
    profileProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    oAuth2Properties :: Prelude.Maybe OAuth2Properties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileProperties', 'customConnectorProfileProperties_profileProperties' - A map of properties that are required to create a profile for the custom
-- connector.
--
-- 'oAuth2Properties', 'customConnectorProfileProperties_oAuth2Properties' - Undocumented member.
newCustomConnectorProfileProperties ::
  CustomConnectorProfileProperties
newCustomConnectorProfileProperties =
  CustomConnectorProfileProperties'
    { profileProperties =
        Prelude.Nothing,
      oAuth2Properties = Prelude.Nothing
    }

-- | A map of properties that are required to create a profile for the custom
-- connector.
customConnectorProfileProperties_profileProperties :: Lens.Lens' CustomConnectorProfileProperties (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
customConnectorProfileProperties_profileProperties = Lens.lens (\CustomConnectorProfileProperties' {profileProperties} -> profileProperties) (\s@CustomConnectorProfileProperties' {} a -> s {profileProperties = a} :: CustomConnectorProfileProperties) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
customConnectorProfileProperties_oAuth2Properties :: Lens.Lens' CustomConnectorProfileProperties (Prelude.Maybe OAuth2Properties)
customConnectorProfileProperties_oAuth2Properties = Lens.lens (\CustomConnectorProfileProperties' {oAuth2Properties} -> oAuth2Properties) (\s@CustomConnectorProfileProperties' {} a -> s {oAuth2Properties = a} :: CustomConnectorProfileProperties)

instance
  Data.FromJSON
    CustomConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "CustomConnectorProfileProperties"
      ( \x ->
          CustomConnectorProfileProperties'
            Prelude.<$> ( x Data..:? "profileProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "oAuth2Properties")
      )

instance
  Prelude.Hashable
    CustomConnectorProfileProperties
  where
  hashWithSalt
    _salt
    CustomConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` profileProperties
        `Prelude.hashWithSalt` oAuth2Properties

instance
  Prelude.NFData
    CustomConnectorProfileProperties
  where
  rnf CustomConnectorProfileProperties' {..} =
    Prelude.rnf profileProperties
      `Prelude.seq` Prelude.rnf oAuth2Properties

instance Data.ToJSON CustomConnectorProfileProperties where
  toJSON CustomConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("profileProperties" Data..=)
              Prelude.<$> profileProperties,
            ("oAuth2Properties" Data..=)
              Prelude.<$> oAuth2Properties
          ]
      )
