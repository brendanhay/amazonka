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
-- Module      : Network.AWS.ELB.Types.SourceSecurityGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.SourceSecurityGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens

-- | Information about a source security group.
--
-- /See:/ 'newSourceSecurityGroup' smart constructor.
data SourceSecurityGroup = SourceSecurityGroup'
  { -- | The owner of the security group.
    ownerAlias :: Core.Maybe Core.Text,
    -- | The name of the security group.
    groupName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceSecurityGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerAlias', 'sourceSecurityGroup_ownerAlias' - The owner of the security group.
--
-- 'groupName', 'sourceSecurityGroup_groupName' - The name of the security group.
newSourceSecurityGroup ::
  SourceSecurityGroup
newSourceSecurityGroup =
  SourceSecurityGroup'
    { ownerAlias = Core.Nothing,
      groupName = Core.Nothing
    }

-- | The owner of the security group.
sourceSecurityGroup_ownerAlias :: Lens.Lens' SourceSecurityGroup (Core.Maybe Core.Text)
sourceSecurityGroup_ownerAlias = Lens.lens (\SourceSecurityGroup' {ownerAlias} -> ownerAlias) (\s@SourceSecurityGroup' {} a -> s {ownerAlias = a} :: SourceSecurityGroup)

-- | The name of the security group.
sourceSecurityGroup_groupName :: Lens.Lens' SourceSecurityGroup (Core.Maybe Core.Text)
sourceSecurityGroup_groupName = Lens.lens (\SourceSecurityGroup' {groupName} -> groupName) (\s@SourceSecurityGroup' {} a -> s {groupName = a} :: SourceSecurityGroup)

instance Core.FromXML SourceSecurityGroup where
  parseXML x =
    SourceSecurityGroup'
      Core.<$> (x Core..@? "OwnerAlias")
      Core.<*> (x Core..@? "GroupName")

instance Core.Hashable SourceSecurityGroup

instance Core.NFData SourceSecurityGroup
