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
-- Module      : Network.AWS.CodeDeploy.DeleteApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an application.
module Network.AWS.CodeDeploy.DeleteApplication
  ( -- * Creating a Request
    DeleteApplication (..),
    newDeleteApplication,

    -- * Request Lenses
    deleteApplication_applicationName,

    -- * Destructuring the Response
    DeleteApplicationResponse (..),
    newDeleteApplicationResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @DeleteApplication@ operation.
--
-- /See:/ 'newDeleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text
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
-- 'applicationName', 'deleteApplication_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
newDeleteApplication ::
  -- | 'applicationName'
  Core.Text ->
  DeleteApplication
newDeleteApplication pApplicationName_ =
  DeleteApplication'
    { applicationName =
        pApplicationName_
    }

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
deleteApplication_applicationName :: Lens.Lens' DeleteApplication Core.Text
deleteApplication_applicationName = Lens.lens (\DeleteApplication' {applicationName} -> applicationName) (\s@DeleteApplication' {} a -> s {applicationName = a} :: DeleteApplication)

instance Core.AWSRequest DeleteApplication where
  type
    AWSResponse DeleteApplication =
      DeleteApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteApplicationResponse'

instance Core.Hashable DeleteApplication

instance Core.NFData DeleteApplication

instance Core.ToHeaders DeleteApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.DeleteApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteApplication where
  toJSON DeleteApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath DeleteApplication where
  toPath = Core.const "/"

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
