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
-- Module      : Network.AWS.EKS.Types.NodegroupHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.NodegroupHealth where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.Issue
import qualified Network.AWS.Lens as Lens

-- | An object representing the health status of the node group.
--
-- /See:/ 'newNodegroupHealth' smart constructor.
data NodegroupHealth = NodegroupHealth'
  { -- | Any issues that are associated with the node group.
    issues :: Core.Maybe [Issue]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NodegroupHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'issues', 'nodegroupHealth_issues' - Any issues that are associated with the node group.
newNodegroupHealth ::
  NodegroupHealth
newNodegroupHealth =
  NodegroupHealth' {issues = Core.Nothing}

-- | Any issues that are associated with the node group.
nodegroupHealth_issues :: Lens.Lens' NodegroupHealth (Core.Maybe [Issue])
nodegroupHealth_issues = Lens.lens (\NodegroupHealth' {issues} -> issues) (\s@NodegroupHealth' {} a -> s {issues = a} :: NodegroupHealth) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON NodegroupHealth where
  parseJSON =
    Core.withObject
      "NodegroupHealth"
      ( \x ->
          NodegroupHealth'
            Core.<$> (x Core..:? "issues" Core..!= Core.mempty)
      )

instance Core.Hashable NodegroupHealth

instance Core.NFData NodegroupHealth
