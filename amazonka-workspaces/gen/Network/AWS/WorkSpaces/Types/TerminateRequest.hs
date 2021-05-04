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
-- Module      : Network.AWS.WorkSpaces.Types.TerminateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.TerminateRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the information used to terminate a WorkSpace.
--
-- /See:/ 'newTerminateRequest' smart constructor.
data TerminateRequest = TerminateRequest'
  { -- | The identifier of the WorkSpace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TerminateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'terminateRequest_workspaceId' - The identifier of the WorkSpace.
newTerminateRequest ::
  -- | 'workspaceId'
  Prelude.Text ->
  TerminateRequest
newTerminateRequest pWorkspaceId_ =
  TerminateRequest' {workspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
terminateRequest_workspaceId :: Lens.Lens' TerminateRequest Prelude.Text
terminateRequest_workspaceId = Lens.lens (\TerminateRequest' {workspaceId} -> workspaceId) (\s@TerminateRequest' {} a -> s {workspaceId = a} :: TerminateRequest)

instance Prelude.Hashable TerminateRequest

instance Prelude.NFData TerminateRequest

instance Prelude.ToJSON TerminateRequest where
  toJSON TerminateRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WorkspaceId" Prelude..= workspaceId)
          ]
      )
