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
-- Module      : Amazonka.EKS.Types.AddonHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.AddonHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.AddonIssue
import qualified Amazonka.Prelude as Prelude

-- | The health of the add-on.
--
-- /See:/ 'newAddonHealth' smart constructor.
data AddonHealth = AddonHealth'
  { -- | An object representing the health issues for an add-on.
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
-- 'issues', 'addonHealth_issues' - An object representing the health issues for an add-on.
newAddonHealth ::
  AddonHealth
newAddonHealth =
  AddonHealth' {issues = Prelude.Nothing}

-- | An object representing the health issues for an add-on.
addonHealth_issues :: Lens.Lens' AddonHealth (Prelude.Maybe [AddonIssue])
addonHealth_issues = Lens.lens (\AddonHealth' {issues} -> issues) (\s@AddonHealth' {} a -> s {issues = a} :: AddonHealth) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AddonHealth where
  parseJSON =
    Data.withObject
      "AddonHealth"
      ( \x ->
          AddonHealth'
            Prelude.<$> (x Data..:? "issues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AddonHealth where
  hashWithSalt _salt AddonHealth' {..} =
    _salt `Prelude.hashWithSalt` issues

instance Prelude.NFData AddonHealth where
  rnf AddonHealth' {..} = Prelude.rnf issues
