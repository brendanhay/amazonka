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
-- Module      : Amazonka.MGN.Types.OS
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.OS where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Operating System.
--
-- /See:/ 'newOS' smart constructor.
data OS = OS'
  { -- | OS full string.
    fullString :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OS' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fullString', 'os_fullString' - OS full string.
newOS ::
  OS
newOS = OS' {fullString = Prelude.Nothing}

-- | OS full string.
os_fullString :: Lens.Lens' OS (Prelude.Maybe Prelude.Text)
os_fullString = Lens.lens (\OS' {fullString} -> fullString) (\s@OS' {} a -> s {fullString = a} :: OS)

instance Core.FromJSON OS where
  parseJSON =
    Core.withObject
      "OS"
      (\x -> OS' Prelude.<$> (x Core..:? "fullString"))

instance Prelude.Hashable OS where
  hashWithSalt _salt OS' {..} =
    _salt `Prelude.hashWithSalt` fullString

instance Prelude.NFData OS where
  rnf OS' {..} = Prelude.rnf fullString
