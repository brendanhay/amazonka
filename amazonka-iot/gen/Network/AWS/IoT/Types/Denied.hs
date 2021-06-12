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
-- Module      : Network.AWS.IoT.Types.Denied
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Denied where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ExplicitDeny
import Network.AWS.IoT.Types.ImplicitDeny
import qualified Network.AWS.Lens as Lens

-- | Contains information that denied the authorization.
--
-- /See:/ 'newDenied' smart constructor.
data Denied = Denied'
  { -- | Information that implicitly denies the authorization. When a policy
    -- doesn\'t explicitly deny or allow an action on a resource it is
    -- considered an implicit deny.
    implicitDeny :: Core.Maybe ImplicitDeny,
    -- | Information that explicitly denies the authorization.
    explicitDeny :: Core.Maybe ExplicitDeny
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Denied' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'implicitDeny', 'denied_implicitDeny' - Information that implicitly denies the authorization. When a policy
-- doesn\'t explicitly deny or allow an action on a resource it is
-- considered an implicit deny.
--
-- 'explicitDeny', 'denied_explicitDeny' - Information that explicitly denies the authorization.
newDenied ::
  Denied
newDenied =
  Denied'
    { implicitDeny = Core.Nothing,
      explicitDeny = Core.Nothing
    }

-- | Information that implicitly denies the authorization. When a policy
-- doesn\'t explicitly deny or allow an action on a resource it is
-- considered an implicit deny.
denied_implicitDeny :: Lens.Lens' Denied (Core.Maybe ImplicitDeny)
denied_implicitDeny = Lens.lens (\Denied' {implicitDeny} -> implicitDeny) (\s@Denied' {} a -> s {implicitDeny = a} :: Denied)

-- | Information that explicitly denies the authorization.
denied_explicitDeny :: Lens.Lens' Denied (Core.Maybe ExplicitDeny)
denied_explicitDeny = Lens.lens (\Denied' {explicitDeny} -> explicitDeny) (\s@Denied' {} a -> s {explicitDeny = a} :: Denied)

instance Core.FromJSON Denied where
  parseJSON =
    Core.withObject
      "Denied"
      ( \x ->
          Denied'
            Core.<$> (x Core..:? "implicitDeny")
            Core.<*> (x Core..:? "explicitDeny")
      )

instance Core.Hashable Denied

instance Core.NFData Denied
