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
-- Module      : Network.AWS.Discovery.DeleteApplications
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a list of applications and their associations with configuration
-- items.
module Network.AWS.Discovery.DeleteApplications
  ( -- * Creating a Request
    DeleteApplications (..),
    newDeleteApplications,

    -- * Request Lenses
    deleteApplications_configurationIds,

    -- * Destructuring the Response
    DeleteApplicationsResponse (..),
    newDeleteApplicationsResponse,

    -- * Response Lenses
    deleteApplicationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteApplications' smart constructor.
data DeleteApplications = DeleteApplications'
  { -- | Configuration ID of an application to be deleted.
    configurationIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApplications' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationIds', 'deleteApplications_configurationIds' - Configuration ID of an application to be deleted.
newDeleteApplications ::
  DeleteApplications
newDeleteApplications =
  DeleteApplications' {configurationIds = Core.mempty}

-- | Configuration ID of an application to be deleted.
deleteApplications_configurationIds :: Lens.Lens' DeleteApplications [Core.Text]
deleteApplications_configurationIds = Lens.lens (\DeleteApplications' {configurationIds} -> configurationIds) (\s@DeleteApplications' {} a -> s {configurationIds = a} :: DeleteApplications) Core.. Lens._Coerce

instance Core.AWSRequest DeleteApplications where
  type
    AWSResponse DeleteApplications =
      DeleteApplicationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteApplicationsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteApplications

instance Core.NFData DeleteApplications

instance Core.ToHeaders DeleteApplications where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DeleteApplications" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteApplications where
  toJSON DeleteApplications' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("configurationIds" Core..= configurationIds)
          ]
      )

instance Core.ToPath DeleteApplications where
  toPath = Core.const "/"

instance Core.ToQuery DeleteApplications where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApplicationsResponse' smart constructor.
data DeleteApplicationsResponse = DeleteApplicationsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApplicationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteApplicationsResponse_httpStatus' - The response's http status code.
newDeleteApplicationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteApplicationsResponse
newDeleteApplicationsResponse pHttpStatus_ =
  DeleteApplicationsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteApplicationsResponse_httpStatus :: Lens.Lens' DeleteApplicationsResponse Core.Int
deleteApplicationsResponse_httpStatus = Lens.lens (\DeleteApplicationsResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationsResponse' {} a -> s {httpStatus = a} :: DeleteApplicationsResponse)

instance Core.NFData DeleteApplicationsResponse
