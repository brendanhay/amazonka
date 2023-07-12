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
-- Module      : Amazonka.WorkSpaces.Types.RebootRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.RebootRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the information used to reboot a WorkSpace.
--
-- /See:/ 'newRebootRequest' smart constructor.
data RebootRequest = RebootRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RebootRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'rebootRequest_workspaceId' - The identifier of the WorkSpace.
newRebootRequest ::
  -- | 'workspaceId'
  Prelude.Text ->
  RebootRequest
newRebootRequest pWorkspaceId_ =
  RebootRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
rebootRequest_workspaceId :: Lens.Lens' RebootRequest Prelude.Text
rebootRequest_workspaceId = Lens.lens (\RebootRequest' {workspaceId} -> workspaceId) (\s@RebootRequest' {} a -> s {workspaceId = a} :: RebootRequest)

instance Prelude.Hashable RebootRequest where
  hashWithSalt _salt RebootRequest' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData RebootRequest where
  rnf RebootRequest' {..} = Prelude.rnf workspaceId

instance Data.ToJSON RebootRequest where
  toJSON RebootRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WorkspaceId" Data..= workspaceId)]
      )
