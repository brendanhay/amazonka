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
-- Module      : Network.AWS.Redshift.Types.ClusterParameterGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterParameterGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Tag

-- | Describes a parameter group.
--
-- /See:/ 'newClusterParameterGroup' smart constructor.
data ClusterParameterGroup = ClusterParameterGroup'
  { -- | The list of tags for the cluster parameter group.
    tags :: Core.Maybe [Tag],
    -- | The name of the cluster parameter group.
    parameterGroupName :: Core.Maybe Core.Text,
    -- | The description of the parameter group.
    description :: Core.Maybe Core.Text,
    -- | The name of the cluster parameter group family that this cluster
    -- parameter group is compatible with.
    parameterGroupFamily :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'clusterParameterGroup_tags' - The list of tags for the cluster parameter group.
--
-- 'parameterGroupName', 'clusterParameterGroup_parameterGroupName' - The name of the cluster parameter group.
--
-- 'description', 'clusterParameterGroup_description' - The description of the parameter group.
--
-- 'parameterGroupFamily', 'clusterParameterGroup_parameterGroupFamily' - The name of the cluster parameter group family that this cluster
-- parameter group is compatible with.
newClusterParameterGroup ::
  ClusterParameterGroup
newClusterParameterGroup =
  ClusterParameterGroup'
    { tags = Core.Nothing,
      parameterGroupName = Core.Nothing,
      description = Core.Nothing,
      parameterGroupFamily = Core.Nothing
    }

-- | The list of tags for the cluster parameter group.
clusterParameterGroup_tags :: Lens.Lens' ClusterParameterGroup (Core.Maybe [Tag])
clusterParameterGroup_tags = Lens.lens (\ClusterParameterGroup' {tags} -> tags) (\s@ClusterParameterGroup' {} a -> s {tags = a} :: ClusterParameterGroup) Core.. Lens.mapping Lens._Coerce

-- | The name of the cluster parameter group.
clusterParameterGroup_parameterGroupName :: Lens.Lens' ClusterParameterGroup (Core.Maybe Core.Text)
clusterParameterGroup_parameterGroupName = Lens.lens (\ClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroup' {} a -> s {parameterGroupName = a} :: ClusterParameterGroup)

-- | The description of the parameter group.
clusterParameterGroup_description :: Lens.Lens' ClusterParameterGroup (Core.Maybe Core.Text)
clusterParameterGroup_description = Lens.lens (\ClusterParameterGroup' {description} -> description) (\s@ClusterParameterGroup' {} a -> s {description = a} :: ClusterParameterGroup)

-- | The name of the cluster parameter group family that this cluster
-- parameter group is compatible with.
clusterParameterGroup_parameterGroupFamily :: Lens.Lens' ClusterParameterGroup (Core.Maybe Core.Text)
clusterParameterGroup_parameterGroupFamily = Lens.lens (\ClusterParameterGroup' {parameterGroupFamily} -> parameterGroupFamily) (\s@ClusterParameterGroup' {} a -> s {parameterGroupFamily = a} :: ClusterParameterGroup)

instance Core.FromXML ClusterParameterGroup where
  parseXML x =
    ClusterParameterGroup'
      Core.<$> ( x Core..@? "Tags" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Tag")
               )
      Core.<*> (x Core..@? "ParameterGroupName")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "ParameterGroupFamily")

instance Core.Hashable ClusterParameterGroup

instance Core.NFData ClusterParameterGroup
