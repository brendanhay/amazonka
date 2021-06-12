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
-- Module      : Network.AWS.RDS.Types.OptionGroupMembership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupMembership where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information on the option groups the DB instance is a member
-- of.
--
-- /See:/ 'newOptionGroupMembership' smart constructor.
data OptionGroupMembership = OptionGroupMembership'
  { -- | The status of the DB instance\'s option group membership. Valid values
    -- are: @in-sync@, @pending-apply@, @pending-removal@,
    -- @pending-maintenance-apply@, @pending-maintenance-removal@, @applying@,
    -- @removing@, and @failed@.
    status :: Core.Maybe Core.Text,
    -- | The name of the option group that the instance belongs to.
    optionGroupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OptionGroupMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'optionGroupMembership_status' - The status of the DB instance\'s option group membership. Valid values
-- are: @in-sync@, @pending-apply@, @pending-removal@,
-- @pending-maintenance-apply@, @pending-maintenance-removal@, @applying@,
-- @removing@, and @failed@.
--
-- 'optionGroupName', 'optionGroupMembership_optionGroupName' - The name of the option group that the instance belongs to.
newOptionGroupMembership ::
  OptionGroupMembership
newOptionGroupMembership =
  OptionGroupMembership'
    { status = Core.Nothing,
      optionGroupName = Core.Nothing
    }

-- | The status of the DB instance\'s option group membership. Valid values
-- are: @in-sync@, @pending-apply@, @pending-removal@,
-- @pending-maintenance-apply@, @pending-maintenance-removal@, @applying@,
-- @removing@, and @failed@.
optionGroupMembership_status :: Lens.Lens' OptionGroupMembership (Core.Maybe Core.Text)
optionGroupMembership_status = Lens.lens (\OptionGroupMembership' {status} -> status) (\s@OptionGroupMembership' {} a -> s {status = a} :: OptionGroupMembership)

-- | The name of the option group that the instance belongs to.
optionGroupMembership_optionGroupName :: Lens.Lens' OptionGroupMembership (Core.Maybe Core.Text)
optionGroupMembership_optionGroupName = Lens.lens (\OptionGroupMembership' {optionGroupName} -> optionGroupName) (\s@OptionGroupMembership' {} a -> s {optionGroupName = a} :: OptionGroupMembership)

instance Core.FromXML OptionGroupMembership where
  parseXML x =
    OptionGroupMembership'
      Core.<$> (x Core..@? "Status")
      Core.<*> (x Core..@? "OptionGroupName")

instance Core.Hashable OptionGroupMembership

instance Core.NFData OptionGroupMembership
