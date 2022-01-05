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
-- Module      : Amazonka.Chime.Types.Origination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Origination where

import Amazonka.Chime.Types.OriginationRoute
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Origination settings enable your SIP hosts to receive inbound calls
-- using your Amazon Chime Voice Connector.
--
-- The parameters listed below are not required, but you must use at least
-- one.
--
-- /See:/ 'newOrigination' smart constructor.
data Origination = Origination'
  { -- | The call distribution properties defined for your SIP hosts. Valid
    -- range: Minimum value of 1. Maximum value of 20. This parameter is not
    -- required, but you must specify this parameter or @Disabled@.
    routes :: Prelude.Maybe [OriginationRoute],
    -- | When origination settings are disabled, inbound calls are not enabled
    -- for your Amazon Chime Voice Connector. This parameter is not required,
    -- but you must specify this parameter or @Routes@.
    disabled :: Prelude.Maybe Prelude.Bool
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
-- 'routes', 'origination_routes' - The call distribution properties defined for your SIP hosts. Valid
-- range: Minimum value of 1. Maximum value of 20. This parameter is not
-- required, but you must specify this parameter or @Disabled@.
--
-- 'disabled', 'origination_disabled' - When origination settings are disabled, inbound calls are not enabled
-- for your Amazon Chime Voice Connector. This parameter is not required,
-- but you must specify this parameter or @Routes@.
newOrigination ::
  Origination
newOrigination =
  Origination'
    { routes = Prelude.Nothing,
      disabled = Prelude.Nothing
    }

-- | The call distribution properties defined for your SIP hosts. Valid
-- range: Minimum value of 1. Maximum value of 20. This parameter is not
-- required, but you must specify this parameter or @Disabled@.
origination_routes :: Lens.Lens' Origination (Prelude.Maybe [OriginationRoute])
origination_routes = Lens.lens (\Origination' {routes} -> routes) (\s@Origination' {} a -> s {routes = a} :: Origination) Prelude.. Lens.mapping Lens.coerced

-- | When origination settings are disabled, inbound calls are not enabled
-- for your Amazon Chime Voice Connector. This parameter is not required,
-- but you must specify this parameter or @Routes@.
origination_disabled :: Lens.Lens' Origination (Prelude.Maybe Prelude.Bool)
origination_disabled = Lens.lens (\Origination' {disabled} -> disabled) (\s@Origination' {} a -> s {disabled = a} :: Origination)

instance Core.FromJSON Origination where
  parseJSON =
    Core.withObject
      "Origination"
      ( \x ->
          Origination'
            Prelude.<$> (x Core..:? "Routes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Disabled")
      )

instance Prelude.Hashable Origination where
  hashWithSalt _salt Origination' {..} =
    _salt `Prelude.hashWithSalt` routes
      `Prelude.hashWithSalt` disabled

instance Prelude.NFData Origination where
  rnf Origination' {..} =
    Prelude.rnf routes
      `Prelude.seq` Prelude.rnf disabled

instance Core.ToJSON Origination where
  toJSON Origination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Routes" Core..=) Prelude.<$> routes,
            ("Disabled" Core..=) Prelude.<$> disabled
          ]
      )
