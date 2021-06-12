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
-- Module      : Network.AWS.EC2.DeleteLaunchTemplateVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more versions of a launch template. You cannot delete the
-- default version of a launch template; you must first assign a different
-- version as the default. If the default version is the only version for
-- the launch template, you must delete the entire launch template using
-- DeleteLaunchTemplate.
module Network.AWS.EC2.DeleteLaunchTemplateVersions
  ( -- * Creating a Request
    DeleteLaunchTemplateVersions (..),
    newDeleteLaunchTemplateVersions,

    -- * Request Lenses
    deleteLaunchTemplateVersions_dryRun,
    deleteLaunchTemplateVersions_launchTemplateId,
    deleteLaunchTemplateVersions_launchTemplateName,
    deleteLaunchTemplateVersions_versions,

    -- * Destructuring the Response
    DeleteLaunchTemplateVersionsResponse (..),
    newDeleteLaunchTemplateVersionsResponse,

    -- * Response Lenses
    deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLaunchTemplateVersions' smart constructor.
data DeleteLaunchTemplateVersions = DeleteLaunchTemplateVersions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateId :: Core.Maybe Core.Text,
    -- | The name of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateName :: Core.Maybe Core.Text,
    -- | The version numbers of one or more launch template versions to delete.
    versions :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteLaunchTemplateVersions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplateId', 'deleteLaunchTemplateVersions_launchTemplateId' - The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'launchTemplateName', 'deleteLaunchTemplateVersions_launchTemplateName' - The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'versions', 'deleteLaunchTemplateVersions_versions' - The version numbers of one or more launch template versions to delete.
newDeleteLaunchTemplateVersions ::
  DeleteLaunchTemplateVersions
newDeleteLaunchTemplateVersions =
  DeleteLaunchTemplateVersions'
    { dryRun =
        Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      versions = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLaunchTemplateVersions_dryRun :: Lens.Lens' DeleteLaunchTemplateVersions (Core.Maybe Core.Bool)
deleteLaunchTemplateVersions_dryRun = Lens.lens (\DeleteLaunchTemplateVersions' {dryRun} -> dryRun) (\s@DeleteLaunchTemplateVersions' {} a -> s {dryRun = a} :: DeleteLaunchTemplateVersions)

-- | The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
deleteLaunchTemplateVersions_launchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersions (Core.Maybe Core.Text)
deleteLaunchTemplateVersions_launchTemplateId = Lens.lens (\DeleteLaunchTemplateVersions' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplateVersions' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersions)

-- | The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
deleteLaunchTemplateVersions_launchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersions (Core.Maybe Core.Text)
deleteLaunchTemplateVersions_launchTemplateName = Lens.lens (\DeleteLaunchTemplateVersions' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplateVersions' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersions)

-- | The version numbers of one or more launch template versions to delete.
deleteLaunchTemplateVersions_versions :: Lens.Lens' DeleteLaunchTemplateVersions [Core.Text]
deleteLaunchTemplateVersions_versions = Lens.lens (\DeleteLaunchTemplateVersions' {versions} -> versions) (\s@DeleteLaunchTemplateVersions' {} a -> s {versions = a} :: DeleteLaunchTemplateVersions) Core.. Lens._Coerce

instance Core.AWSRequest DeleteLaunchTemplateVersions where
  type
    AWSResponse DeleteLaunchTemplateVersions =
      DeleteLaunchTemplateVersionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateVersionsResponse'
            Core.<$> ( x
                         Core..@? "unsuccessfullyDeletedLaunchTemplateVersionSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> ( x
                         Core..@? "successfullyDeletedLaunchTemplateVersionSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLaunchTemplateVersions

instance Core.NFData DeleteLaunchTemplateVersions

instance Core.ToHeaders DeleteLaunchTemplateVersions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteLaunchTemplateVersions where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLaunchTemplateVersions where
  toQuery DeleteLaunchTemplateVersions' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteLaunchTemplateVersions" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "LaunchTemplateName" Core.=: launchTemplateName,
        Core.toQueryList "LaunchTemplateVersion" versions
      ]

-- | /See:/ 'newDeleteLaunchTemplateVersionsResponse' smart constructor.
data DeleteLaunchTemplateVersionsResponse = DeleteLaunchTemplateVersionsResponse'
  { -- | Information about the launch template versions that could not be
    -- deleted.
    unsuccessfullyDeletedLaunchTemplateVersions :: Core.Maybe [DeleteLaunchTemplateVersionsResponseErrorItem],
    -- | Information about the launch template versions that were successfully
    -- deleted.
    successfullyDeletedLaunchTemplateVersions :: Core.Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessfullyDeletedLaunchTemplateVersions', 'deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that could not be
-- deleted.
--
-- 'successfullyDeletedLaunchTemplateVersions', 'deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that were successfully
-- deleted.
--
-- 'httpStatus', 'deleteLaunchTemplateVersionsResponse_httpStatus' - The response's http status code.
newDeleteLaunchTemplateVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLaunchTemplateVersionsResponse
newDeleteLaunchTemplateVersionsResponse pHttpStatus_ =
  DeleteLaunchTemplateVersionsResponse'
    { unsuccessfullyDeletedLaunchTemplateVersions =
        Core.Nothing,
      successfullyDeletedLaunchTemplateVersions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template versions that could not be
-- deleted.
deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Core.Maybe [DeleteLaunchTemplateVersionsResponseErrorItem])
deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions = Lens.lens (\DeleteLaunchTemplateVersionsResponse' {unsuccessfullyDeletedLaunchTemplateVersions} -> unsuccessfullyDeletedLaunchTemplateVersions) (\s@DeleteLaunchTemplateVersionsResponse' {} a -> s {unsuccessfullyDeletedLaunchTemplateVersions = a} :: DeleteLaunchTemplateVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | Information about the launch template versions that were successfully
-- deleted.
deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Core.Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem])
deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions = Lens.lens (\DeleteLaunchTemplateVersionsResponse' {successfullyDeletedLaunchTemplateVersions} -> successfullyDeletedLaunchTemplateVersions) (\s@DeleteLaunchTemplateVersionsResponse' {} a -> s {successfullyDeletedLaunchTemplateVersions = a} :: DeleteLaunchTemplateVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteLaunchTemplateVersionsResponse_httpStatus :: Lens.Lens' DeleteLaunchTemplateVersionsResponse Core.Int
deleteLaunchTemplateVersionsResponse_httpStatus = Lens.lens (\DeleteLaunchTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@DeleteLaunchTemplateVersionsResponse' {} a -> s {httpStatus = a} :: DeleteLaunchTemplateVersionsResponse)

instance
  Core.NFData
    DeleteLaunchTemplateVersionsResponse
