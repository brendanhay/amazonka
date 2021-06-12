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
-- Module      : Network.AWS.MediaLive.Types.InputWhitelistRuleCidr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputWhitelistRuleCidr where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An IPv4 CIDR to whitelist.
--
-- /See:/ 'newInputWhitelistRuleCidr' smart constructor.
data InputWhitelistRuleCidr = InputWhitelistRuleCidr'
  { -- | The IPv4 CIDR to whitelist.
    cidr :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputWhitelistRuleCidr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'inputWhitelistRuleCidr_cidr' - The IPv4 CIDR to whitelist.
newInputWhitelistRuleCidr ::
  InputWhitelistRuleCidr
newInputWhitelistRuleCidr =
  InputWhitelistRuleCidr' {cidr = Core.Nothing}

-- | The IPv4 CIDR to whitelist.
inputWhitelistRuleCidr_cidr :: Lens.Lens' InputWhitelistRuleCidr (Core.Maybe Core.Text)
inputWhitelistRuleCidr_cidr = Lens.lens (\InputWhitelistRuleCidr' {cidr} -> cidr) (\s@InputWhitelistRuleCidr' {} a -> s {cidr = a} :: InputWhitelistRuleCidr)

instance Core.Hashable InputWhitelistRuleCidr

instance Core.NFData InputWhitelistRuleCidr

instance Core.ToJSON InputWhitelistRuleCidr where
  toJSON InputWhitelistRuleCidr' {..} =
    Core.object
      (Core.catMaybes [("cidr" Core..=) Core.<$> cidr])
