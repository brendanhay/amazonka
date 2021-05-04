{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Mobile.ExportProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports project configuration to a snapshot which can be downloaded and
-- shared. Note that mobile app push credentials are encrypted in exported
-- projects, so they can only be shared successfully within the same AWS
-- account.
module Network.AWS.Mobile.ExportProject
  ( -- * Creating a Request
    ExportProject (..),
    newExportProject,

    -- * Request Lenses
    exportProject_projectId,

    -- * Destructuring the Response
    ExportProjectResponse (..),
    newExportProjectResponse,

    -- * Response Lenses
    exportProjectResponse_downloadUrl,
    exportProjectResponse_snapshotId,
    exportProjectResponse_shareUrl,
    exportProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used in requests to export project configuration
-- details.
--
-- /See:/ 'newExportProject' smart constructor.
data ExportProject = ExportProject'
  { -- | Unique project identifier.
    projectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectId', 'exportProject_projectId' - Unique project identifier.
newExportProject ::
  -- | 'projectId'
  Prelude.Text ->
  ExportProject
newExportProject pProjectId_ =
  ExportProject' {projectId = pProjectId_}

-- | Unique project identifier.
exportProject_projectId :: Lens.Lens' ExportProject Prelude.Text
exportProject_projectId = Lens.lens (\ExportProject' {projectId} -> projectId) (\s@ExportProject' {} a -> s {projectId = a} :: ExportProject)

instance Prelude.AWSRequest ExportProject where
  type Rs ExportProject = ExportProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportProjectResponse'
            Prelude.<$> (x Prelude..?> "downloadUrl")
            Prelude.<*> (x Prelude..?> "snapshotId")
            Prelude.<*> (x Prelude..?> "shareUrl")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportProject

instance Prelude.NFData ExportProject

instance Prelude.ToHeaders ExportProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ExportProject where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath ExportProject where
  toPath ExportProject' {..} =
    Prelude.mconcat
      ["/exports/", Prelude.toBS projectId]

instance Prelude.ToQuery ExportProject where
  toQuery = Prelude.const Prelude.mempty

-- | Result structure used for requests to export project configuration
-- details.
--
-- /See:/ 'newExportProjectResponse' smart constructor.
data ExportProjectResponse = ExportProjectResponse'
  { -- | URL which can be used to download the exported project configuation
    -- file(s).
    downloadUrl :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for the exported snapshot of the project
    -- configuration. This snapshot identifier is included in the share URL.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | URL which can be shared to allow other AWS users to create their own
    -- project in AWS Mobile Hub with the same configuration as the specified
    -- project. This URL pertains to a snapshot in time of the project
    -- configuration that is created when this API is called. If you want to
    -- share additional changes to your project configuration, then you will
    -- need to create and share a new snapshot by calling this method again.
    shareUrl :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExportProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadUrl', 'exportProjectResponse_downloadUrl' - URL which can be used to download the exported project configuation
-- file(s).
--
-- 'snapshotId', 'exportProjectResponse_snapshotId' - Unique identifier for the exported snapshot of the project
-- configuration. This snapshot identifier is included in the share URL.
--
-- 'shareUrl', 'exportProjectResponse_shareUrl' - URL which can be shared to allow other AWS users to create their own
-- project in AWS Mobile Hub with the same configuration as the specified
-- project. This URL pertains to a snapshot in time of the project
-- configuration that is created when this API is called. If you want to
-- share additional changes to your project configuration, then you will
-- need to create and share a new snapshot by calling this method again.
--
-- 'httpStatus', 'exportProjectResponse_httpStatus' - The response's http status code.
newExportProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportProjectResponse
newExportProjectResponse pHttpStatus_ =
  ExportProjectResponse'
    { downloadUrl =
        Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      shareUrl = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | URL which can be used to download the exported project configuation
-- file(s).
exportProjectResponse_downloadUrl :: Lens.Lens' ExportProjectResponse (Prelude.Maybe Prelude.Text)
exportProjectResponse_downloadUrl = Lens.lens (\ExportProjectResponse' {downloadUrl} -> downloadUrl) (\s@ExportProjectResponse' {} a -> s {downloadUrl = a} :: ExportProjectResponse)

-- | Unique identifier for the exported snapshot of the project
-- configuration. This snapshot identifier is included in the share URL.
exportProjectResponse_snapshotId :: Lens.Lens' ExportProjectResponse (Prelude.Maybe Prelude.Text)
exportProjectResponse_snapshotId = Lens.lens (\ExportProjectResponse' {snapshotId} -> snapshotId) (\s@ExportProjectResponse' {} a -> s {snapshotId = a} :: ExportProjectResponse)

-- | URL which can be shared to allow other AWS users to create their own
-- project in AWS Mobile Hub with the same configuration as the specified
-- project. This URL pertains to a snapshot in time of the project
-- configuration that is created when this API is called. If you want to
-- share additional changes to your project configuration, then you will
-- need to create and share a new snapshot by calling this method again.
exportProjectResponse_shareUrl :: Lens.Lens' ExportProjectResponse (Prelude.Maybe Prelude.Text)
exportProjectResponse_shareUrl = Lens.lens (\ExportProjectResponse' {shareUrl} -> shareUrl) (\s@ExportProjectResponse' {} a -> s {shareUrl = a} :: ExportProjectResponse)

-- | The response's http status code.
exportProjectResponse_httpStatus :: Lens.Lens' ExportProjectResponse Prelude.Int
exportProjectResponse_httpStatus = Lens.lens (\ExportProjectResponse' {httpStatus} -> httpStatus) (\s@ExportProjectResponse' {} a -> s {httpStatus = a} :: ExportProjectResponse)

instance Prelude.NFData ExportProjectResponse
