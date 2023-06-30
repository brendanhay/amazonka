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
-- Module      : Amazonka.Redshift.Types.ClusterParameterGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterParameterGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag

-- | Describes a parameter group.
--
-- /See:/ 'newClusterParameterGroup' smart constructor.
data ClusterParameterGroup = ClusterParameterGroup'
  { -- | The description of the parameter group.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster parameter group family that this cluster
    -- parameter group is compatible with.
    parameterGroupFamily :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster parameter group.
    parameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The list of tags for the cluster parameter group.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterParameterGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'clusterParameterGroup_description' - The description of the parameter group.
--
-- 'parameterGroupFamily', 'clusterParameterGroup_parameterGroupFamily' - The name of the cluster parameter group family that this cluster
-- parameter group is compatible with.
--
-- 'parameterGroupName', 'clusterParameterGroup_parameterGroupName' - The name of the cluster parameter group.
--
-- 'tags', 'clusterParameterGroup_tags' - The list of tags for the cluster parameter group.
newClusterParameterGroup ::
  ClusterParameterGroup
newClusterParameterGroup =
  ClusterParameterGroup'
    { description =
        Prelude.Nothing,
      parameterGroupFamily = Prelude.Nothing,
      parameterGroupName = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The description of the parameter group.
clusterParameterGroup_description :: Lens.Lens' ClusterParameterGroup (Prelude.Maybe Prelude.Text)
clusterParameterGroup_description = Lens.lens (\ClusterParameterGroup' {description} -> description) (\s@ClusterParameterGroup' {} a -> s {description = a} :: ClusterParameterGroup)

-- | The name of the cluster parameter group family that this cluster
-- parameter group is compatible with.
clusterParameterGroup_parameterGroupFamily :: Lens.Lens' ClusterParameterGroup (Prelude.Maybe Prelude.Text)
clusterParameterGroup_parameterGroupFamily = Lens.lens (\ClusterParameterGroup' {parameterGroupFamily} -> parameterGroupFamily) (\s@ClusterParameterGroup' {} a -> s {parameterGroupFamily = a} :: ClusterParameterGroup)

-- | The name of the cluster parameter group.
clusterParameterGroup_parameterGroupName :: Lens.Lens' ClusterParameterGroup (Prelude.Maybe Prelude.Text)
clusterParameterGroup_parameterGroupName = Lens.lens (\ClusterParameterGroup' {parameterGroupName} -> parameterGroupName) (\s@ClusterParameterGroup' {} a -> s {parameterGroupName = a} :: ClusterParameterGroup)

-- | The list of tags for the cluster parameter group.
clusterParameterGroup_tags :: Lens.Lens' ClusterParameterGroup (Prelude.Maybe [Tag])
clusterParameterGroup_tags = Lens.lens (\ClusterParameterGroup' {tags} -> tags) (\s@ClusterParameterGroup' {} a -> s {tags = a} :: ClusterParameterGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ClusterParameterGroup where
  parseXML x =
    ClusterParameterGroup'
      Prelude.<$> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "ParameterGroupFamily")
      Prelude.<*> (x Data..@? "ParameterGroupName")
      Prelude.<*> ( x
                      Data..@? "Tags"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Tag")
                  )

instance Prelude.Hashable ClusterParameterGroup where
  hashWithSalt _salt ClusterParameterGroup' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterGroupFamily
      `Prelude.hashWithSalt` parameterGroupName
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ClusterParameterGroup where
  rnf ClusterParameterGroup' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterGroupFamily
      `Prelude.seq` Prelude.rnf parameterGroupName
      `Prelude.seq` Prelude.rnf tags
