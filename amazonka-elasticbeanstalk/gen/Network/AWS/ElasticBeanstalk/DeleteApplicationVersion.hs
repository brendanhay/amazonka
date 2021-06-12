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
-- Module      : Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version from the specified application.
--
-- You cannot delete an application version that is associated with a
-- running environment.
module Network.AWS.ElasticBeanstalk.DeleteApplicationVersion
  ( -- * Creating a Request
    DeleteApplicationVersion (..),
    newDeleteApplicationVersion,

    -- * Request Lenses
    deleteApplicationVersion_deleteSourceBundle,
    deleteApplicationVersion_applicationName,
    deleteApplicationVersion_versionLabel,

    -- * Destructuring the Response
    DeleteApplicationVersionResponse (..),
    newDeleteApplicationVersionResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to delete an application version.
--
-- /See:/ 'newDeleteApplicationVersion' smart constructor.
data DeleteApplicationVersion = DeleteApplicationVersion'
  { -- | Set to @true@ to delete the source bundle from your storage bucket.
    -- Otherwise, the application version is deleted only from Elastic
    -- Beanstalk and the source bundle remains in Amazon S3.
    deleteSourceBundle :: Core.Maybe Core.Bool,
    -- | The name of the application to which the version belongs.
    applicationName :: Core.Text,
    -- | The label of the version to delete.
    versionLabel :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteSourceBundle', 'deleteApplicationVersion_deleteSourceBundle' - Set to @true@ to delete the source bundle from your storage bucket.
-- Otherwise, the application version is deleted only from Elastic
-- Beanstalk and the source bundle remains in Amazon S3.
--
-- 'applicationName', 'deleteApplicationVersion_applicationName' - The name of the application to which the version belongs.
--
-- 'versionLabel', 'deleteApplicationVersion_versionLabel' - The label of the version to delete.
newDeleteApplicationVersion ::
  -- | 'applicationName'
  Core.Text ->
  -- | 'versionLabel'
  Core.Text ->
  DeleteApplicationVersion
newDeleteApplicationVersion
  pApplicationName_
  pVersionLabel_ =
    DeleteApplicationVersion'
      { deleteSourceBundle =
          Core.Nothing,
        applicationName = pApplicationName_,
        versionLabel = pVersionLabel_
      }

-- | Set to @true@ to delete the source bundle from your storage bucket.
-- Otherwise, the application version is deleted only from Elastic
-- Beanstalk and the source bundle remains in Amazon S3.
deleteApplicationVersion_deleteSourceBundle :: Lens.Lens' DeleteApplicationVersion (Core.Maybe Core.Bool)
deleteApplicationVersion_deleteSourceBundle = Lens.lens (\DeleteApplicationVersion' {deleteSourceBundle} -> deleteSourceBundle) (\s@DeleteApplicationVersion' {} a -> s {deleteSourceBundle = a} :: DeleteApplicationVersion)

-- | The name of the application to which the version belongs.
deleteApplicationVersion_applicationName :: Lens.Lens' DeleteApplicationVersion Core.Text
deleteApplicationVersion_applicationName = Lens.lens (\DeleteApplicationVersion' {applicationName} -> applicationName) (\s@DeleteApplicationVersion' {} a -> s {applicationName = a} :: DeleteApplicationVersion)

-- | The label of the version to delete.
deleteApplicationVersion_versionLabel :: Lens.Lens' DeleteApplicationVersion Core.Text
deleteApplicationVersion_versionLabel = Lens.lens (\DeleteApplicationVersion' {versionLabel} -> versionLabel) (\s@DeleteApplicationVersion' {} a -> s {versionLabel = a} :: DeleteApplicationVersion)

instance Core.AWSRequest DeleteApplicationVersion where
  type
    AWSResponse DeleteApplicationVersion =
      DeleteApplicationVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteApplicationVersionResponse'

instance Core.Hashable DeleteApplicationVersion

instance Core.NFData DeleteApplicationVersion

instance Core.ToHeaders DeleteApplicationVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteApplicationVersion where
  toPath = Core.const "/"

instance Core.ToQuery DeleteApplicationVersion where
  toQuery DeleteApplicationVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteApplicationVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "DeleteSourceBundle" Core.=: deleteSourceBundle,
        "ApplicationName" Core.=: applicationName,
        "VersionLabel" Core.=: versionLabel
      ]

-- | /See:/ 'newDeleteApplicationVersionResponse' smart constructor.
data DeleteApplicationVersionResponse = DeleteApplicationVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteApplicationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteApplicationVersionResponse ::
  DeleteApplicationVersionResponse
newDeleteApplicationVersionResponse =
  DeleteApplicationVersionResponse'

instance Core.NFData DeleteApplicationVersionResponse
