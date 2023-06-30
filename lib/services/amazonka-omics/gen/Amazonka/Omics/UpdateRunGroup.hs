{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Omics.UpdateRunGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a run group.
module Amazonka.Omics.UpdateRunGroup
  ( -- * Creating a Request
    UpdateRunGroup (..),
    newUpdateRunGroup,

    -- * Request Lenses
    updateRunGroup_maxCpus,
    updateRunGroup_maxDuration,
    updateRunGroup_maxRuns,
    updateRunGroup_name,
    updateRunGroup_id,

    -- * Destructuring the Response
    UpdateRunGroupResponse (..),
    newUpdateRunGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRunGroup' smart constructor.
data UpdateRunGroup = UpdateRunGroup'
  { -- | The maximum number of CPUs to use.
    maxCpus :: Prelude.Maybe Prelude.Natural,
    -- | The maximum amount of time to run.
    maxDuration :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of concurrent runs for the group.
    maxRuns :: Prelude.Maybe Prelude.Natural,
    -- | A name for the group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The group\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRunGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxCpus', 'updateRunGroup_maxCpus' - The maximum number of CPUs to use.
--
-- 'maxDuration', 'updateRunGroup_maxDuration' - The maximum amount of time to run.
--
-- 'maxRuns', 'updateRunGroup_maxRuns' - The maximum number of concurrent runs for the group.
--
-- 'name', 'updateRunGroup_name' - A name for the group.
--
-- 'id', 'updateRunGroup_id' - The group\'s ID.
newUpdateRunGroup ::
  -- | 'id'
  Prelude.Text ->
  UpdateRunGroup
newUpdateRunGroup pId_ =
  UpdateRunGroup'
    { maxCpus = Prelude.Nothing,
      maxDuration = Prelude.Nothing,
      maxRuns = Prelude.Nothing,
      name = Prelude.Nothing,
      id = pId_
    }

-- | The maximum number of CPUs to use.
updateRunGroup_maxCpus :: Lens.Lens' UpdateRunGroup (Prelude.Maybe Prelude.Natural)
updateRunGroup_maxCpus = Lens.lens (\UpdateRunGroup' {maxCpus} -> maxCpus) (\s@UpdateRunGroup' {} a -> s {maxCpus = a} :: UpdateRunGroup)

-- | The maximum amount of time to run.
updateRunGroup_maxDuration :: Lens.Lens' UpdateRunGroup (Prelude.Maybe Prelude.Natural)
updateRunGroup_maxDuration = Lens.lens (\UpdateRunGroup' {maxDuration} -> maxDuration) (\s@UpdateRunGroup' {} a -> s {maxDuration = a} :: UpdateRunGroup)

-- | The maximum number of concurrent runs for the group.
updateRunGroup_maxRuns :: Lens.Lens' UpdateRunGroup (Prelude.Maybe Prelude.Natural)
updateRunGroup_maxRuns = Lens.lens (\UpdateRunGroup' {maxRuns} -> maxRuns) (\s@UpdateRunGroup' {} a -> s {maxRuns = a} :: UpdateRunGroup)

-- | A name for the group.
updateRunGroup_name :: Lens.Lens' UpdateRunGroup (Prelude.Maybe Prelude.Text)
updateRunGroup_name = Lens.lens (\UpdateRunGroup' {name} -> name) (\s@UpdateRunGroup' {} a -> s {name = a} :: UpdateRunGroup)

-- | The group\'s ID.
updateRunGroup_id :: Lens.Lens' UpdateRunGroup Prelude.Text
updateRunGroup_id = Lens.lens (\UpdateRunGroup' {id} -> id) (\s@UpdateRunGroup' {} a -> s {id = a} :: UpdateRunGroup)

instance Core.AWSRequest UpdateRunGroup where
  type
    AWSResponse UpdateRunGroup =
      UpdateRunGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateRunGroupResponse'

instance Prelude.Hashable UpdateRunGroup where
  hashWithSalt _salt UpdateRunGroup' {..} =
    _salt
      `Prelude.hashWithSalt` maxCpus
      `Prelude.hashWithSalt` maxDuration
      `Prelude.hashWithSalt` maxRuns
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateRunGroup where
  rnf UpdateRunGroup' {..} =
    Prelude.rnf maxCpus
      `Prelude.seq` Prelude.rnf maxDuration
      `Prelude.seq` Prelude.rnf maxRuns
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateRunGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRunGroup where
  toJSON UpdateRunGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxCpus" Data..=) Prelude.<$> maxCpus,
            ("maxDuration" Data..=) Prelude.<$> maxDuration,
            ("maxRuns" Data..=) Prelude.<$> maxRuns,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateRunGroup where
  toPath UpdateRunGroup' {..} =
    Prelude.mconcat ["/runGroup/", Data.toBS id]

instance Data.ToQuery UpdateRunGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRunGroupResponse' smart constructor.
data UpdateRunGroupResponse = UpdateRunGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRunGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRunGroupResponse ::
  UpdateRunGroupResponse
newUpdateRunGroupResponse = UpdateRunGroupResponse'

instance Prelude.NFData UpdateRunGroupResponse where
  rnf _ = ()
