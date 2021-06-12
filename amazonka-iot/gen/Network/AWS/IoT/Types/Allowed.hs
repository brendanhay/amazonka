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
-- Module      : Network.AWS.IoT.Types.Allowed
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Allowed where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Policy
import qualified Network.AWS.Lens as Lens

-- | Contains information that allowed the authorization.
--
-- /See:/ 'newAllowed' smart constructor.
data Allowed = Allowed'
  { -- | A list of policies that allowed the authentication.
    policies :: Core.Maybe [Policy]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Allowed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'allowed_policies' - A list of policies that allowed the authentication.
newAllowed ::
  Allowed
newAllowed = Allowed' {policies = Core.Nothing}

-- | A list of policies that allowed the authentication.
allowed_policies :: Lens.Lens' Allowed (Core.Maybe [Policy])
allowed_policies = Lens.lens (\Allowed' {policies} -> policies) (\s@Allowed' {} a -> s {policies = a} :: Allowed) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Allowed where
  parseJSON =
    Core.withObject
      "Allowed"
      ( \x ->
          Allowed'
            Core.<$> (x Core..:? "policies" Core..!= Core.mempty)
      )

instance Core.Hashable Allowed

instance Core.NFData Allowed
