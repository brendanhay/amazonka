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
-- Module      : Amazonka.MediaLive.Types.InputWhitelistRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputWhitelistRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Whitelist rule
--
-- /See:/ 'newInputWhitelistRule' smart constructor.
data InputWhitelistRule = InputWhitelistRule'
  { -- | The IPv4 CIDR that\'s whitelisted.
    cidr :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputWhitelistRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidr', 'inputWhitelistRule_cidr' - The IPv4 CIDR that\'s whitelisted.
newInputWhitelistRule ::
  InputWhitelistRule
newInputWhitelistRule =
  InputWhitelistRule' {cidr = Prelude.Nothing}

-- | The IPv4 CIDR that\'s whitelisted.
inputWhitelistRule_cidr :: Lens.Lens' InputWhitelistRule (Prelude.Maybe Prelude.Text)
inputWhitelistRule_cidr = Lens.lens (\InputWhitelistRule' {cidr} -> cidr) (\s@InputWhitelistRule' {} a -> s {cidr = a} :: InputWhitelistRule)

instance Data.FromJSON InputWhitelistRule where
  parseJSON =
    Data.withObject
      "InputWhitelistRule"
      ( \x ->
          InputWhitelistRule' Prelude.<$> (x Data..:? "cidr")
      )

instance Prelude.Hashable InputWhitelistRule where
  hashWithSalt _salt InputWhitelistRule' {..} =
    _salt `Prelude.hashWithSalt` cidr

instance Prelude.NFData InputWhitelistRule where
  rnf InputWhitelistRule' {..} = Prelude.rnf cidr
