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
-- Module      : Amazonka.ResilienceHub.Types.AppComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.AppComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines an Application Component.
--
-- /See:/ 'newAppComponent' smart constructor.
data AppComponent = AppComponent'
  { -- | Additional configuration parameters for an Resilience Hub application.
    -- If you want to implement @additionalInfo@ through the Resilience Hub
    -- console rather than using an API call, see
    -- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
    --
    -- Currently, this parameter accepts a key-value mapping (in a string
    -- format) of only one failover region and one associated account.
    --
    -- Key: @\"failover-regions\"@
    --
    -- Value:
    -- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
    additionalInfo :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)),
    -- | Unique identifier of the Application Component.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the Application Component.
    name :: Prelude.Text,
    -- | The type of Application Component.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalInfo', 'appComponent_additionalInfo' - Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter accepts a key-value mapping (in a string
-- format) of only one failover region and one associated account.
--
-- Key: @\"failover-regions\"@
--
-- Value:
-- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
--
-- 'id', 'appComponent_id' - Unique identifier of the Application Component.
--
-- 'name', 'appComponent_name' - The name of the Application Component.
--
-- 'type'', 'appComponent_type' - The type of Application Component.
newAppComponent ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  AppComponent
newAppComponent pName_ pType_ =
  AppComponent'
    { additionalInfo = Prelude.Nothing,
      id = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | Additional configuration parameters for an Resilience Hub application.
-- If you want to implement @additionalInfo@ through the Resilience Hub
-- console rather than using an API call, see
-- <https://docs.aws.amazon.com/resilience-hub/latest/userguide/app-config-param.html Configure the application configuration parameters>.
--
-- Currently, this parameter accepts a key-value mapping (in a string
-- format) of only one failover region and one associated account.
--
-- Key: @\"failover-regions\"@
--
-- Value:
-- @\"[{\"region\":\"\<REGION>\", \"accounts\":[{\"id\":\"\<ACCOUNT_ID>\"}]}]\"@
appComponent_additionalInfo :: Lens.Lens' AppComponent (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.NonEmpty Prelude.Text)))
appComponent_additionalInfo = Lens.lens (\AppComponent' {additionalInfo} -> additionalInfo) (\s@AppComponent' {} a -> s {additionalInfo = a} :: AppComponent) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier of the Application Component.
appComponent_id :: Lens.Lens' AppComponent (Prelude.Maybe Prelude.Text)
appComponent_id = Lens.lens (\AppComponent' {id} -> id) (\s@AppComponent' {} a -> s {id = a} :: AppComponent)

-- | The name of the Application Component.
appComponent_name :: Lens.Lens' AppComponent Prelude.Text
appComponent_name = Lens.lens (\AppComponent' {name} -> name) (\s@AppComponent' {} a -> s {name = a} :: AppComponent)

-- | The type of Application Component.
appComponent_type :: Lens.Lens' AppComponent Prelude.Text
appComponent_type = Lens.lens (\AppComponent' {type'} -> type') (\s@AppComponent' {} a -> s {type' = a} :: AppComponent)

instance Data.FromJSON AppComponent where
  parseJSON =
    Data.withObject
      "AppComponent"
      ( \x ->
          AppComponent'
            Prelude.<$> (x Data..:? "additionalInfo" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AppComponent where
  hashWithSalt _salt AppComponent' {..} =
    _salt
      `Prelude.hashWithSalt` additionalInfo
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AppComponent where
  rnf AppComponent' {..} =
    Prelude.rnf additionalInfo
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
