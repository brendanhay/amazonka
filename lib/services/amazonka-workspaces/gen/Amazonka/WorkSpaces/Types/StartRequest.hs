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
-- Module      : Amazonka.WorkSpaces.Types.StartRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.StartRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information used to start a WorkSpace.
--
-- /See:/ 'newStartRequest' smart constructor.
data StartRequest = StartRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'startRequest_workspaceId' - The identifier of the WorkSpace.
newStartRequest ::
  StartRequest
newStartRequest =
  StartRequest' {workspaceId = Prelude.Nothing}

-- | The identifier of the WorkSpace.
startRequest_workspaceId :: Lens.Lens' StartRequest (Prelude.Maybe Prelude.Text)
startRequest_workspaceId = Lens.lens (\StartRequest' {workspaceId} -> workspaceId) (\s@StartRequest' {} a -> s {workspaceId = a} :: StartRequest)

instance Prelude.Hashable StartRequest where
  hashWithSalt _salt StartRequest' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData StartRequest where
  rnf StartRequest' {..} = Prelude.rnf workspaceId

instance Data.ToJSON StartRequest where
  toJSON StartRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("WorkspaceId" Data..=) Prelude.<$> workspaceId]
      )
