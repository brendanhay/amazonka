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
-- Module      : Network.AWS.Mobile.ExportBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates customized software development kit (SDK) and or tool packages
-- used to integrate mobile web or mobile app clients with backend AWS
-- resources.
module Network.AWS.Mobile.ExportBundle
  ( -- * Creating a Request
    ExportBundle (..),
    newExportBundle,

    -- * Request Lenses
    exportBundle_platform,
    exportBundle_projectId,
    exportBundle_bundleId,

    -- * Destructuring the Response
    ExportBundleResponse (..),
    newExportBundleResponse,

    -- * Response Lenses
    exportBundleResponse_downloadUrl,
    exportBundleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request generation of custom SDK and tool
-- packages required to integrate mobile web or app clients with backed AWS
-- resources.
--
-- /See:/ 'newExportBundle' smart constructor.
data ExportBundle = ExportBundle'
  { -- | Developer desktop or target application platform.
    platform :: Core.Maybe Platform,
    -- | Unique project identifier.
    projectId :: Core.Maybe Core.Text,
    -- | Unique bundle identifier.
    bundleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'exportBundle_platform' - Developer desktop or target application platform.
--
-- 'projectId', 'exportBundle_projectId' - Unique project identifier.
--
-- 'bundleId', 'exportBundle_bundleId' - Unique bundle identifier.
newExportBundle ::
  -- | 'bundleId'
  Core.Text ->
  ExportBundle
newExportBundle pBundleId_ =
  ExportBundle'
    { platform = Core.Nothing,
      projectId = Core.Nothing,
      bundleId = pBundleId_
    }

-- | Developer desktop or target application platform.
exportBundle_platform :: Lens.Lens' ExportBundle (Core.Maybe Platform)
exportBundle_platform = Lens.lens (\ExportBundle' {platform} -> platform) (\s@ExportBundle' {} a -> s {platform = a} :: ExportBundle)

-- | Unique project identifier.
exportBundle_projectId :: Lens.Lens' ExportBundle (Core.Maybe Core.Text)
exportBundle_projectId = Lens.lens (\ExportBundle' {projectId} -> projectId) (\s@ExportBundle' {} a -> s {projectId = a} :: ExportBundle)

-- | Unique bundle identifier.
exportBundle_bundleId :: Lens.Lens' ExportBundle Core.Text
exportBundle_bundleId = Lens.lens (\ExportBundle' {bundleId} -> bundleId) (\s@ExportBundle' {} a -> s {bundleId = a} :: ExportBundle)

instance Core.AWSRequest ExportBundle where
  type AWSResponse ExportBundle = ExportBundleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportBundleResponse'
            Core.<$> (x Core..?> "downloadUrl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ExportBundle

instance Core.NFData ExportBundle

instance Core.ToHeaders ExportBundle where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ExportBundle where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath ExportBundle where
  toPath ExportBundle' {..} =
    Core.mconcat ["/bundles/", Core.toBS bundleId]

instance Core.ToQuery ExportBundle where
  toQuery ExportBundle' {..} =
    Core.mconcat
      [ "platform" Core.=: platform,
        "projectId" Core.=: projectId
      ]

-- | Result structure which contains link to download custom-generated SDK
-- and tool packages used to integrate mobile web or app clients with
-- backed AWS resources.
--
-- /See:/ 'newExportBundleResponse' smart constructor.
data ExportBundleResponse = ExportBundleResponse'
  { -- | URL which contains the custom-generated SDK and tool packages used to
    -- integrate the client mobile app or web app with the AWS resources
    -- created by the AWS Mobile Hub project.
    downloadUrl :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ExportBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'downloadUrl', 'exportBundleResponse_downloadUrl' - URL which contains the custom-generated SDK and tool packages used to
-- integrate the client mobile app or web app with the AWS resources
-- created by the AWS Mobile Hub project.
--
-- 'httpStatus', 'exportBundleResponse_httpStatus' - The response's http status code.
newExportBundleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ExportBundleResponse
newExportBundleResponse pHttpStatus_ =
  ExportBundleResponse'
    { downloadUrl = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | URL which contains the custom-generated SDK and tool packages used to
-- integrate the client mobile app or web app with the AWS resources
-- created by the AWS Mobile Hub project.
exportBundleResponse_downloadUrl :: Lens.Lens' ExportBundleResponse (Core.Maybe Core.Text)
exportBundleResponse_downloadUrl = Lens.lens (\ExportBundleResponse' {downloadUrl} -> downloadUrl) (\s@ExportBundleResponse' {} a -> s {downloadUrl = a} :: ExportBundleResponse)

-- | The response's http status code.
exportBundleResponse_httpStatus :: Lens.Lens' ExportBundleResponse Core.Int
exportBundleResponse_httpStatus = Lens.lens (\ExportBundleResponse' {httpStatus} -> httpStatus) (\s@ExportBundleResponse' {} a -> s {httpStatus = a} :: ExportBundleResponse)

instance Core.NFData ExportBundleResponse
