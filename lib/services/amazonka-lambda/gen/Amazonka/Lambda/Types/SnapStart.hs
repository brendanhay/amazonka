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
-- Module      : Amazonka.Lambda.Types.SnapStart
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.SnapStart where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.SnapStartApplyOn
import qualified Amazonka.Prelude as Prelude

-- | The function\'s SnapStart setting. Set @ApplyOn@ to @PublishedVersions@
-- to create a snapshot of the initialized execution environment when you
-- publish a function version. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/snapstart.html Reducing startup time with Lambda SnapStart>.
--
-- /See:/ 'newSnapStart' smart constructor.
data SnapStart = SnapStart'
  { -- | Set to @PublishedVersions@ to create a snapshot of the initialized
    -- execution environment when you publish a function version.
    applyOn :: Prelude.Maybe SnapStartApplyOn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SnapStart' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyOn', 'snapStart_applyOn' - Set to @PublishedVersions@ to create a snapshot of the initialized
-- execution environment when you publish a function version.
newSnapStart ::
  SnapStart
newSnapStart = SnapStart' {applyOn = Prelude.Nothing}

-- | Set to @PublishedVersions@ to create a snapshot of the initialized
-- execution environment when you publish a function version.
snapStart_applyOn :: Lens.Lens' SnapStart (Prelude.Maybe SnapStartApplyOn)
snapStart_applyOn = Lens.lens (\SnapStart' {applyOn} -> applyOn) (\s@SnapStart' {} a -> s {applyOn = a} :: SnapStart)

instance Prelude.Hashable SnapStart where
  hashWithSalt _salt SnapStart' {..} =
    _salt `Prelude.hashWithSalt` applyOn

instance Prelude.NFData SnapStart where
  rnf SnapStart' {..} = Prelude.rnf applyOn

instance Data.ToJSON SnapStart where
  toJSON SnapStart' {..} =
    Data.object
      ( Prelude.catMaybes
          [("ApplyOn" Data..=) Prelude.<$> applyOn]
      )
