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
-- Module      : Network.AWS.IoT.Types.ImplicitDeny
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ImplicitDeny where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens

-- | Information that implicitly denies authorization. When policy doesn\'t
-- explicitly deny or allow an action on a resource it is considered an
-- implicit deny.
--
-- /See:/ 'newImplicitDeny' smart constructor.
data ImplicitDeny = ImplicitDeny'
  { -- | Policies that don\'t contain a matching allow or deny statement for the
    -- specified action on the specified resource.
    policies :: Core.Maybe [Policy]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ImplicitDeny' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'implicitDeny_policies' - Policies that don\'t contain a matching allow or deny statement for the
-- specified action on the specified resource.
newImplicitDeny ::
  ImplicitDeny
newImplicitDeny =
  ImplicitDeny' {policies = Core.Nothing}

-- | Policies that don\'t contain a matching allow or deny statement for the
-- specified action on the specified resource.
implicitDeny_policies :: Lens.Lens' ImplicitDeny (Core.Maybe [Policy])
implicitDeny_policies = Lens.lens (\ImplicitDeny' {policies} -> policies) (\s@ImplicitDeny' {} a -> s {policies = a} :: ImplicitDeny) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ImplicitDeny where
  parseJSON =
    Core.withObject
      "ImplicitDeny"
      ( \x ->
          ImplicitDeny'
            Core.<$> (x Core..:? "policies" Core..!= Core.mempty)
      )

instance Core.Hashable ImplicitDeny

instance Core.NFData ImplicitDeny
