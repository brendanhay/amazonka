{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EKS.Types.Issue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object representing the health status of the node group.
--
-- /See:/ 'newNodegroupHealth' smart constructor.
data NodegroupHealth = NodegroupHealth'
  { -- | Any issues that are associated with the node group.
    issues :: Prelude.Maybe [Issue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  NodegroupHealth' {issues = Prelude.Nothing}

-- | Any issues that are associated with the node group.
nodegroupHealth_issues :: Lens.Lens' NodegroupHealth (Prelude.Maybe [Issue])
nodegroupHealth_issues = Lens.lens (\NodegroupHealth' {issues} -> issues) (\s@NodegroupHealth' {} a -> s {issues = a} :: NodegroupHealth) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON NodegroupHealth where
  parseJSON =
    Prelude.withObject
      "NodegroupHealth"
      ( \x ->
          NodegroupHealth'
            Prelude.<$> (x Prelude..:? "issues" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable NodegroupHealth

instance Prelude.NFData NodegroupHealth
