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
-- Module      : Network.AWS.IoT.Types.ExplicitDeny
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ExplicitDeny where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens

-- | Information that explicitly denies authorization.
--
-- /See:/ 'newExplicitDeny' smart constructor.
data ExplicitDeny = ExplicitDeny'
  { -- | The policies that denied the authorization.
    policies :: Core.Maybe [Policy]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExplicitDeny' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'explicitDeny_policies' - The policies that denied the authorization.
newExplicitDeny ::
  ExplicitDeny
newExplicitDeny =
  ExplicitDeny' {policies = Core.Nothing}

-- | The policies that denied the authorization.
explicitDeny_policies :: Lens.Lens' ExplicitDeny (Core.Maybe [Policy])
explicitDeny_policies = Lens.lens (\ExplicitDeny' {policies} -> policies) (\s@ExplicitDeny' {} a -> s {policies = a} :: ExplicitDeny) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ExplicitDeny where
  parseJSON =
    Core.withObject
      "ExplicitDeny"
      ( \x ->
          ExplicitDeny'
            Core.<$> (x Core..:? "policies" Core..!= Core.mempty)
      )

instance Core.Hashable ExplicitDeny

instance Core.NFData ExplicitDeny
