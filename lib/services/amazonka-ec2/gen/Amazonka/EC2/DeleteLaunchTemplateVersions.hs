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
-- Module      : Amazonka.EC2.DeleteLaunchTemplateVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more versions of a launch template. You cannot delete the
-- default version of a launch template; you must first assign a different
-- version as the default. If the default version is the only version for
-- the launch template, you must delete the entire launch template using
-- DeleteLaunchTemplate.
module Amazonka.EC2.DeleteLaunchTemplateVersions
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
    deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions,
    deleteLaunchTemplateVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLaunchTemplateVersions' smart constructor.
data DeleteLaunchTemplateVersions = DeleteLaunchTemplateVersions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the launch template.
    --
    -- You must specify either the @LaunchTemplateId@ or the
    -- @LaunchTemplateName@, but not both.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    --
    -- You must specify either the @LaunchTemplateName@ or the
    -- @LaunchTemplateId@, but not both.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version numbers of one or more launch template versions to delete.
    versions :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'launchTemplateId', 'deleteLaunchTemplateVersions_launchTemplateId' - The ID of the launch template.
--
-- You must specify either the @LaunchTemplateId@ or the
-- @LaunchTemplateName@, but not both.
--
-- 'launchTemplateName', 'deleteLaunchTemplateVersions_launchTemplateName' - The name of the launch template.
--
-- You must specify either the @LaunchTemplateName@ or the
-- @LaunchTemplateId@, but not both.
--
-- 'versions', 'deleteLaunchTemplateVersions_versions' - The version numbers of one or more launch template versions to delete.
newDeleteLaunchTemplateVersions ::
  DeleteLaunchTemplateVersions
newDeleteLaunchTemplateVersions =
  DeleteLaunchTemplateVersions'
    { dryRun =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      versions = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLaunchTemplateVersions_dryRun :: Lens.Lens' DeleteLaunchTemplateVersions (Prelude.Maybe Prelude.Bool)
deleteLaunchTemplateVersions_dryRun = Lens.lens (\DeleteLaunchTemplateVersions' {dryRun} -> dryRun) (\s@DeleteLaunchTemplateVersions' {} a -> s {dryRun = a} :: DeleteLaunchTemplateVersions)

-- | The ID of the launch template.
--
-- You must specify either the @LaunchTemplateId@ or the
-- @LaunchTemplateName@, but not both.
deleteLaunchTemplateVersions_launchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersions_launchTemplateId = Lens.lens (\DeleteLaunchTemplateVersions' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplateVersions' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersions)

-- | The name of the launch template.
--
-- You must specify either the @LaunchTemplateName@ or the
-- @LaunchTemplateId@, but not both.
deleteLaunchTemplateVersions_launchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersions (Prelude.Maybe Prelude.Text)
deleteLaunchTemplateVersions_launchTemplateName = Lens.lens (\DeleteLaunchTemplateVersions' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplateVersions' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersions)

-- | The version numbers of one or more launch template versions to delete.
deleteLaunchTemplateVersions_versions :: Lens.Lens' DeleteLaunchTemplateVersions [Prelude.Text]
deleteLaunchTemplateVersions_versions = Lens.lens (\DeleteLaunchTemplateVersions' {versions} -> versions) (\s@DeleteLaunchTemplateVersions' {} a -> s {versions = a} :: DeleteLaunchTemplateVersions) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteLaunchTemplateVersions where
  type
    AWSResponse DeleteLaunchTemplateVersions =
      DeleteLaunchTemplateVersionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateVersionsResponse'
            Prelude.<$> ( x
                            Data..@? "successfullyDeletedLaunchTemplateVersionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> ( x
                            Data..@? "unsuccessfullyDeletedLaunchTemplateVersionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteLaunchTemplateVersions
  where
  hashWithSalt _salt DeleteLaunchTemplateVersions' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` versions

instance Prelude.NFData DeleteLaunchTemplateVersions where
  rnf DeleteLaunchTemplateVersions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf versions

instance Data.ToHeaders DeleteLaunchTemplateVersions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteLaunchTemplateVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLaunchTemplateVersions where
  toQuery DeleteLaunchTemplateVersions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeleteLaunchTemplateVersions" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "LaunchTemplateId" Data.=: launchTemplateId,
        "LaunchTemplateName" Data.=: launchTemplateName,
        Data.toQueryList "LaunchTemplateVersion" versions
      ]

-- | /See:/ 'newDeleteLaunchTemplateVersionsResponse' smart constructor.
data DeleteLaunchTemplateVersionsResponse = DeleteLaunchTemplateVersionsResponse'
  { -- | Information about the launch template versions that were successfully
    -- deleted.
    successfullyDeletedLaunchTemplateVersions :: Prelude.Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem],
    -- | Information about the launch template versions that could not be
    -- deleted.
    unsuccessfullyDeletedLaunchTemplateVersions :: Prelude.Maybe [DeleteLaunchTemplateVersionsResponseErrorItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'successfullyDeletedLaunchTemplateVersions', 'deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that were successfully
-- deleted.
--
-- 'unsuccessfullyDeletedLaunchTemplateVersions', 'deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions' - Information about the launch template versions that could not be
-- deleted.
--
-- 'httpStatus', 'deleteLaunchTemplateVersionsResponse_httpStatus' - The response's http status code.
newDeleteLaunchTemplateVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLaunchTemplateVersionsResponse
newDeleteLaunchTemplateVersionsResponse pHttpStatus_ =
  DeleteLaunchTemplateVersionsResponse'
    { successfullyDeletedLaunchTemplateVersions =
        Prelude.Nothing,
      unsuccessfullyDeletedLaunchTemplateVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template versions that were successfully
-- deleted.
deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Prelude.Maybe [DeleteLaunchTemplateVersionsResponseSuccessItem])
deleteLaunchTemplateVersionsResponse_successfullyDeletedLaunchTemplateVersions = Lens.lens (\DeleteLaunchTemplateVersionsResponse' {successfullyDeletedLaunchTemplateVersions} -> successfullyDeletedLaunchTemplateVersions) (\s@DeleteLaunchTemplateVersionsResponse' {} a -> s {successfullyDeletedLaunchTemplateVersions = a} :: DeleteLaunchTemplateVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the launch template versions that could not be
-- deleted.
deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions :: Lens.Lens' DeleteLaunchTemplateVersionsResponse (Prelude.Maybe [DeleteLaunchTemplateVersionsResponseErrorItem])
deleteLaunchTemplateVersionsResponse_unsuccessfullyDeletedLaunchTemplateVersions = Lens.lens (\DeleteLaunchTemplateVersionsResponse' {unsuccessfullyDeletedLaunchTemplateVersions} -> unsuccessfullyDeletedLaunchTemplateVersions) (\s@DeleteLaunchTemplateVersionsResponse' {} a -> s {unsuccessfullyDeletedLaunchTemplateVersions = a} :: DeleteLaunchTemplateVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteLaunchTemplateVersionsResponse_httpStatus :: Lens.Lens' DeleteLaunchTemplateVersionsResponse Prelude.Int
deleteLaunchTemplateVersionsResponse_httpStatus = Lens.lens (\DeleteLaunchTemplateVersionsResponse' {httpStatus} -> httpStatus) (\s@DeleteLaunchTemplateVersionsResponse' {} a -> s {httpStatus = a} :: DeleteLaunchTemplateVersionsResponse)

instance
  Prelude.NFData
    DeleteLaunchTemplateVersionsResponse
  where
  rnf DeleteLaunchTemplateVersionsResponse' {..} =
    Prelude.rnf
      successfullyDeletedLaunchTemplateVersions
      `Prelude.seq` Prelude.rnf
        unsuccessfullyDeletedLaunchTemplateVersions
      `Prelude.seq` Prelude.rnf httpStatus
