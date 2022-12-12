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
-- Module      : Amazonka.ChimeSdkVoice.Types.Origination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.Origination where

import Amazonka.ChimeSdkVoice.Types.OriginationRoute
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newOrigination' smart constructor.
data Origination = Origination'
  { disabled :: Prelude.Maybe Prelude.Bool,
    routes :: Prelude.Maybe [OriginationRoute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Origination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'origination_disabled' - Undocumented member.
--
-- 'routes', 'origination_routes' - Undocumented member.
newOrigination ::
  Origination
newOrigination =
  Origination'
    { disabled = Prelude.Nothing,
      routes = Prelude.Nothing
    }

-- | Undocumented member.
origination_disabled :: Lens.Lens' Origination (Prelude.Maybe Prelude.Bool)
origination_disabled = Lens.lens (\Origination' {disabled} -> disabled) (\s@Origination' {} a -> s {disabled = a} :: Origination)

-- | Undocumented member.
origination_routes :: Lens.Lens' Origination (Prelude.Maybe [OriginationRoute])
origination_routes = Lens.lens (\Origination' {routes} -> routes) (\s@Origination' {} a -> s {routes = a} :: Origination) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Origination where
  parseJSON =
    Data.withObject
      "Origination"
      ( \x ->
          Origination'
            Prelude.<$> (x Data..:? "Disabled")
            Prelude.<*> (x Data..:? "Routes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Origination where
  hashWithSalt _salt Origination' {..} =
    _salt `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` routes

instance Prelude.NFData Origination where
  rnf Origination' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf routes

instance Data.ToJSON Origination where
  toJSON Origination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Disabled" Data..=) Prelude.<$> disabled,
            ("Routes" Data..=) Prelude.<$> routes
          ]
      )
