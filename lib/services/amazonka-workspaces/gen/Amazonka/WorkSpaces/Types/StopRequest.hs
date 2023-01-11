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
-- Module      : Amazonka.WorkSpaces.Types.StopRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.StopRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the information used to stop a WorkSpace.
--
-- /See:/ 'newStopRequest' smart constructor.
data StopRequest = StopRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'stopRequest_workspaceId' - The identifier of the WorkSpace.
newStopRequest ::
  StopRequest
newStopRequest =
  StopRequest' {workspaceId = Prelude.Nothing}

-- | The identifier of the WorkSpace.
stopRequest_workspaceId :: Lens.Lens' StopRequest (Prelude.Maybe Prelude.Text)
stopRequest_workspaceId = Lens.lens (\StopRequest' {workspaceId} -> workspaceId) (\s@StopRequest' {} a -> s {workspaceId = a} :: StopRequest)

instance Prelude.Hashable StopRequest where
  hashWithSalt _salt StopRequest' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData StopRequest where
  rnf StopRequest' {..} = Prelude.rnf workspaceId

instance Data.ToJSON StopRequest where
  toJSON StopRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("WorkspaceId" Data..=) Prelude.<$> workspaceId]
      )
