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
-- Module      : Network.AWS.EKS.Types.AddonHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.AddonHealth where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.AddonIssue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The health of the add-on.
--
-- /See:/ 'newAddonHealth' smart constructor.
data AddonHealth = AddonHealth'
  { -- | An object that represents the add-on\'s health issues.
    issues :: Prelude.Maybe [AddonIssue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddonHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issues', 'addonHealth_issues' - An object that represents the add-on\'s health issues.
newAddonHealth ::
  AddonHealth
newAddonHealth =
  AddonHealth' {issues = Prelude.Nothing}

-- | An object that represents the add-on\'s health issues.
addonHealth_issues :: Lens.Lens' AddonHealth (Prelude.Maybe [AddonIssue])
addonHealth_issues = Lens.lens (\AddonHealth' {issues} -> issues) (\s@AddonHealth' {} a -> s {issues = a} :: AddonHealth) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AddonHealth where
  parseJSON =
    Core.withObject
      "AddonHealth"
      ( \x ->
          AddonHealth'
            Prelude.<$> (x Core..:? "issues" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AddonHealth

instance Prelude.NFData AddonHealth
