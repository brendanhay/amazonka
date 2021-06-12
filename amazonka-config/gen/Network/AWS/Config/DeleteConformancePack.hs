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
-- Module      : Network.AWS.Config.DeleteConformancePack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified conformance pack and all the AWS Config rules,
-- remediation actions, and all evaluation results within that conformance
-- pack.
--
-- AWS Config sets the conformance pack to @DELETE_IN_PROGRESS@ until the
-- deletion is complete. You cannot update a conformance pack while it is
-- in this state.
module Network.AWS.Config.DeleteConformancePack
  ( -- * Creating a Request
    DeleteConformancePack (..),
    newDeleteConformancePack,

    -- * Request Lenses
    deleteConformancePack_conformancePackName,

    -- * Destructuring the Response
    DeleteConformancePackResponse (..),
    newDeleteConformancePackResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteConformancePack' smart constructor.
data DeleteConformancePack = DeleteConformancePack'
  { -- | Name of the conformance pack you want to delete.
    conformancePackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConformancePack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackName', 'deleteConformancePack_conformancePackName' - Name of the conformance pack you want to delete.
newDeleteConformancePack ::
  -- | 'conformancePackName'
  Core.Text ->
  DeleteConformancePack
newDeleteConformancePack pConformancePackName_ =
  DeleteConformancePack'
    { conformancePackName =
        pConformancePackName_
    }

-- | Name of the conformance pack you want to delete.
deleteConformancePack_conformancePackName :: Lens.Lens' DeleteConformancePack Core.Text
deleteConformancePack_conformancePackName = Lens.lens (\DeleteConformancePack' {conformancePackName} -> conformancePackName) (\s@DeleteConformancePack' {} a -> s {conformancePackName = a} :: DeleteConformancePack)

instance Core.AWSRequest DeleteConformancePack where
  type
    AWSResponse DeleteConformancePack =
      DeleteConformancePackResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteConformancePackResponse'

instance Core.Hashable DeleteConformancePack

instance Core.NFData DeleteConformancePack

instance Core.ToHeaders DeleteConformancePack where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteConformancePack" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteConformancePack where
  toJSON DeleteConformancePack' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ConformancePackName" Core..= conformancePackName)
          ]
      )

instance Core.ToPath DeleteConformancePack where
  toPath = Core.const "/"

instance Core.ToQuery DeleteConformancePack where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteConformancePackResponse' smart constructor.
data DeleteConformancePackResponse = DeleteConformancePackResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteConformancePackResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteConformancePackResponse ::
  DeleteConformancePackResponse
newDeleteConformancePackResponse =
  DeleteConformancePackResponse'

instance Core.NFData DeleteConformancePackResponse
