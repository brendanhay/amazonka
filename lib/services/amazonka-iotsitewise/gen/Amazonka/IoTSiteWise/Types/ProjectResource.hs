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
-- Module      : Amazonka.IoTSiteWise.Types.ProjectResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ProjectResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies a specific IoT SiteWise Monitor project.
--
-- /See:/ 'newProjectResource' smart constructor.
data ProjectResource = ProjectResource'
  { -- | The ID of the project.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'projectResource_id' - The ID of the project.
newProjectResource ::
  -- | 'id'
  Prelude.Text ->
  ProjectResource
newProjectResource pId_ = ProjectResource' {id = pId_}

-- | The ID of the project.
projectResource_id :: Lens.Lens' ProjectResource Prelude.Text
projectResource_id = Lens.lens (\ProjectResource' {id} -> id) (\s@ProjectResource' {} a -> s {id = a} :: ProjectResource)

instance Data.FromJSON ProjectResource where
  parseJSON =
    Data.withObject
      "ProjectResource"
      ( \x ->
          ProjectResource' Prelude.<$> (x Data..: "id")
      )

instance Prelude.Hashable ProjectResource where
  hashWithSalt _salt ProjectResource' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData ProjectResource where
  rnf ProjectResource' {..} = Prelude.rnf id

instance Data.ToJSON ProjectResource where
  toJSON ProjectResource' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])
