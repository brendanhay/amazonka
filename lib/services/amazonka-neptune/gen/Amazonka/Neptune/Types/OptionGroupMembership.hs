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
-- Module      : Amazonka.Neptune.Types.OptionGroupMembership
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.OptionGroupMembership where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Not supported by Neptune.
--
-- /See:/ 'newOptionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
  { -- | Not supported by Neptune.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionGroupName', 'optionGroupMembership_optionGroupName' - Not supported by Neptune.
--
-- 'status', 'optionGroupMembership_status' - Not supported by Neptune.
newOptionGroupMembership ::
  OptionGroupMembership
newOptionGroupMembership =
  OptionGroupMembership'
    { optionGroupName =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Not supported by Neptune.
optionGroupMembership_optionGroupName :: Lens.Lens' OptionGroupMembership (Prelude.Maybe Prelude.Text)
optionGroupMembership_optionGroupName = Lens.lens (\OptionGroupMembership' {optionGroupName} -> optionGroupName) (\s@OptionGroupMembership' {} a -> s {optionGroupName = a} :: OptionGroupMembership)

-- | Not supported by Neptune.
optionGroupMembership_status :: Lens.Lens' OptionGroupMembership (Prelude.Maybe Prelude.Text)
optionGroupMembership_status = Lens.lens (\OptionGroupMembership' {status} -> status) (\s@OptionGroupMembership' {} a -> s {status = a} :: OptionGroupMembership)

instance Core.FromXML OptionGroupMembership where
  parseXML x =
    OptionGroupMembership'
      Prelude.<$> (x Core..@? "OptionGroupName")
      Prelude.<*> (x Core..@? "Status")

instance Prelude.Hashable OptionGroupMembership where
  hashWithSalt _salt OptionGroupMembership' {..} =
    _salt `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` status

instance Prelude.NFData OptionGroupMembership where
  rnf OptionGroupMembership' {..} =
    Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf status
