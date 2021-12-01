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
-- Module      : Amazonka.IoTSiteWise.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Resource where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.PortalResource
import Amazonka.IoTSiteWise.Types.ProjectResource
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an IoT SiteWise Monitor resource ID for a portal or project.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | A portal resource.
    portal :: Prelude.Maybe PortalResource,
    -- | A project resource.
    project :: Prelude.Maybe ProjectResource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portal', 'resource_portal' - A portal resource.
--
-- 'project', 'resource_project' - A project resource.
newResource ::
  Resource
newResource =
  Resource'
    { portal = Prelude.Nothing,
      project = Prelude.Nothing
    }

-- | A portal resource.
resource_portal :: Lens.Lens' Resource (Prelude.Maybe PortalResource)
resource_portal = Lens.lens (\Resource' {portal} -> portal) (\s@Resource' {} a -> s {portal = a} :: Resource)

-- | A project resource.
resource_project :: Lens.Lens' Resource (Prelude.Maybe ProjectResource)
resource_project = Lens.lens (\Resource' {project} -> project) (\s@Resource' {} a -> s {project = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "portal")
            Prelude.<*> (x Core..:? "project")
      )

instance Prelude.Hashable Resource where
  hashWithSalt salt' Resource' {..} =
    salt' `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` portal

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf portal
      `Prelude.seq` Prelude.rnf project

instance Core.ToJSON Resource where
  toJSON Resource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("portal" Core..=) Prelude.<$> portal,
            ("project" Core..=) Prelude.<$> project
          ]
      )
