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
-- Module      : Network.AWS.CodeDeploy.RegisterApplicationRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers with AWS CodeDeploy a revision for the specified application.
module Network.AWS.CodeDeploy.RegisterApplicationRevision
  ( -- * Creating a Request
    RegisterApplicationRevision (..),
    newRegisterApplicationRevision,

    -- * Request Lenses
    registerApplicationRevision_description,
    registerApplicationRevision_applicationName,
    registerApplicationRevision_revision,

    -- * Destructuring the Response
    RegisterApplicationRevisionResponse (..),
    newRegisterApplicationRevisionResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a RegisterApplicationRevision operation.
--
-- /See:/ 'newRegisterApplicationRevision' smart constructor.
data RegisterApplicationRevision = RegisterApplicationRevision'
  { -- | A comment about the revision.
    description :: Core.Maybe Core.Text,
    -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text,
    -- | Information about the application revision to register, including type
    -- and location.
    revision :: RevisionLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterApplicationRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'registerApplicationRevision_description' - A comment about the revision.
--
-- 'applicationName', 'registerApplicationRevision_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
--
-- 'revision', 'registerApplicationRevision_revision' - Information about the application revision to register, including type
-- and location.
newRegisterApplicationRevision ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'revision'
  RevisionLocation ->
  RegisterApplicationRevision
newRegisterApplicationRevision
  pApplicationName_
  pRevision_ =
    RegisterApplicationRevision'
      { description =
          Core.Nothing,
        applicationName = pApplicationName_,
        revision = pRevision_
      }

-- | A comment about the revision.
registerApplicationRevision_description :: Lens.Lens' RegisterApplicationRevision (Core.Maybe Core.Text)
registerApplicationRevision_description = Lens.lens (\RegisterApplicationRevision' {description} -> description) (\s@RegisterApplicationRevision' {} a -> s {description = a} :: RegisterApplicationRevision)

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
registerApplicationRevision_applicationName :: Lens.Lens' RegisterApplicationRevision Core.Text
registerApplicationRevision_applicationName = Lens.lens (\RegisterApplicationRevision' {applicationName} -> applicationName) (\s@RegisterApplicationRevision' {} a -> s {applicationName = a} :: RegisterApplicationRevision)

-- | Information about the application revision to register, including type
-- and location.
registerApplicationRevision_revision :: Lens.Lens' RegisterApplicationRevision RevisionLocation
registerApplicationRevision_revision = Lens.lens (\RegisterApplicationRevision' {revision} -> revision) (\s@RegisterApplicationRevision' {} a -> s {revision = a} :: RegisterApplicationRevision)

instance Core.AWSRequest RegisterApplicationRevision where
  type
    AWSResponse RegisterApplicationRevision =
      RegisterApplicationRevisionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      RegisterApplicationRevisionResponse'

instance Core.Hashable RegisterApplicationRevision

instance Core.NFData RegisterApplicationRevision

instance Core.ToHeaders RegisterApplicationRevision where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.RegisterApplicationRevision" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON RegisterApplicationRevision where
  toJSON RegisterApplicationRevision' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            Core.Just
              ("applicationName" Core..= applicationName),
            Core.Just ("revision" Core..= revision)
          ]
      )

instance Core.ToPath RegisterApplicationRevision where
  toPath = Core.const "/"

instance Core.ToQuery RegisterApplicationRevision where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newRegisterApplicationRevisionResponse' smart constructor.
data RegisterApplicationRevisionResponse = RegisterApplicationRevisionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterApplicationRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRegisterApplicationRevisionResponse ::
  RegisterApplicationRevisionResponse
newRegisterApplicationRevisionResponse =
  RegisterApplicationRevisionResponse'

instance
  Core.NFData
    RegisterApplicationRevisionResponse
