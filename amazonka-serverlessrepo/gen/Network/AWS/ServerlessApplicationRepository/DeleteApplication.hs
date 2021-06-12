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
-- Module      : Network.AWS.ServerlessApplicationRepository.DeleteApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application.
module Network.AWS.ServerlessApplicationRepository.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_applicationId,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'deleteApplication_applicationId' - The Amazon Resource Name (ARN) of the application.
newDeleteApplication ::
  -- | 'applicationId'
  Core.Text ->
  DeleteApplication
newDeleteApplication pApplicationId_ =
  DeleteApplication' {applicationId = pApplicationId_}

-- | The Amazon Resource Name (ARN) of the application.
deleteApplication_applicationId :: Lens.Lens' DeleteApplication Core.Text
deleteApplication_applicationId = Lens.lens (\DeleteApplication' {applicationId} -> applicationId) (\s@DeleteApplication' {} a -> s {applicationId = a} :: DeleteApplication)

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteApplicationResponse'

instance Core.Hashable DeleteApplication

instance Core.NFData DeleteApplication

instance Core.ToHeaders DeleteApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteApplication where
  toPath DeleteApplication' {..} =
    Core.mconcat
      ["/applications/", Core.toBS applicationId]

instance Core.ToQuery DeleteApplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteApplicationResponse' smart constructor.
data DeleteApplicationResponse = DeleteApplicationResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApplicationResponse ::
  DeleteApplicationResponse
newDeleteApplicationResponse =
  DeleteApplicationResponse'

instance Core.NFData DeleteApplicationResponse
