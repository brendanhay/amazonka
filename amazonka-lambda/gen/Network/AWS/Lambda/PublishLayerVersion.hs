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
-- Module      : Network.AWS.Lambda.PublishLayerVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer>
-- from a ZIP archive. Each time you call @PublishLayerVersion@ with the
-- same layer name, a new version is created.
--
-- Add layers to your function with CreateFunction or
-- UpdateFunctionConfiguration.
module Network.AWS.Lambda.PublishLayerVersion
  ( -- * Creating a Request
    PublishLayerVersion (..),
    newPublishLayerVersion,

    -- * Request Lenses
    publishLayerVersion_compatibleRuntimes,
    publishLayerVersion_description,
    publishLayerVersion_licenseInfo,
    publishLayerVersion_layerName,
    publishLayerVersion_content,

    -- * Destructuring the Response
    PublishLayerVersionResponse (..),
    newPublishLayerVersionResponse,

    -- * Response Lenses
    publishLayerVersionResponse_createdDate,
    publishLayerVersionResponse_layerArn,
    publishLayerVersionResponse_version,
    publishLayerVersionResponse_layerVersionArn,
    publishLayerVersionResponse_content,
    publishLayerVersionResponse_compatibleRuntimes,
    publishLayerVersionResponse_description,
    publishLayerVersionResponse_licenseInfo,
    publishLayerVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPublishLayerVersion' smart constructor.
data PublishLayerVersion = PublishLayerVersion'
  { -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes>.
    -- Used for filtering with ListLayers and ListLayerVersions.
    compatibleRuntimes :: Core.Maybe [Runtime],
    -- | The description of the version.
    description :: Core.Maybe Core.Text,
    -- | The layer\'s software license. It can be any of the following:
    --
    -- -   An <https://spdx.org/licenses/ SPDX license identifier>. For
    --     example, @MIT@.
    --
    -- -   The URL of a license hosted on the internet. For example,
    --     @https:\/\/opensource.org\/licenses\/MIT@.
    --
    -- -   The full text of the license.
    licenseInfo :: Core.Maybe Core.Text,
    -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Core.Text,
    -- | The function layer archive.
    content :: LayerVersionContentInput
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublishLayerVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleRuntimes', 'publishLayerVersion_compatibleRuntimes' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes>.
-- Used for filtering with ListLayers and ListLayerVersions.
--
-- 'description', 'publishLayerVersion_description' - The description of the version.
--
-- 'licenseInfo', 'publishLayerVersion_licenseInfo' - The layer\'s software license. It can be any of the following:
--
-- -   An <https://spdx.org/licenses/ SPDX license identifier>. For
--     example, @MIT@.
--
-- -   The URL of a license hosted on the internet. For example,
--     @https:\/\/opensource.org\/licenses\/MIT@.
--
-- -   The full text of the license.
--
-- 'layerName', 'publishLayerVersion_layerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- 'content', 'publishLayerVersion_content' - The function layer archive.
newPublishLayerVersion ::
  -- | 'layerName'
  Core.Text ->
  -- | 'content'
  LayerVersionContentInput ->
  PublishLayerVersion
newPublishLayerVersion pLayerName_ pContent_ =
  PublishLayerVersion'
    { compatibleRuntimes =
        Core.Nothing,
      description = Core.Nothing,
      licenseInfo = Core.Nothing,
      layerName = pLayerName_,
      content = pContent_
    }

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes>.
-- Used for filtering with ListLayers and ListLayerVersions.
publishLayerVersion_compatibleRuntimes :: Lens.Lens' PublishLayerVersion (Core.Maybe [Runtime])
publishLayerVersion_compatibleRuntimes = Lens.lens (\PublishLayerVersion' {compatibleRuntimes} -> compatibleRuntimes) (\s@PublishLayerVersion' {} a -> s {compatibleRuntimes = a} :: PublishLayerVersion) Core.. Lens.mapping Lens._Coerce

-- | The description of the version.
publishLayerVersion_description :: Lens.Lens' PublishLayerVersion (Core.Maybe Core.Text)
publishLayerVersion_description = Lens.lens (\PublishLayerVersion' {description} -> description) (\s@PublishLayerVersion' {} a -> s {description = a} :: PublishLayerVersion)

-- | The layer\'s software license. It can be any of the following:
--
-- -   An <https://spdx.org/licenses/ SPDX license identifier>. For
--     example, @MIT@.
--
-- -   The URL of a license hosted on the internet. For example,
--     @https:\/\/opensource.org\/licenses\/MIT@.
--
-- -   The full text of the license.
publishLayerVersion_licenseInfo :: Lens.Lens' PublishLayerVersion (Core.Maybe Core.Text)
publishLayerVersion_licenseInfo = Lens.lens (\PublishLayerVersion' {licenseInfo} -> licenseInfo) (\s@PublishLayerVersion' {} a -> s {licenseInfo = a} :: PublishLayerVersion)

-- | The name or Amazon Resource Name (ARN) of the layer.
publishLayerVersion_layerName :: Lens.Lens' PublishLayerVersion Core.Text
publishLayerVersion_layerName = Lens.lens (\PublishLayerVersion' {layerName} -> layerName) (\s@PublishLayerVersion' {} a -> s {layerName = a} :: PublishLayerVersion)

-- | The function layer archive.
publishLayerVersion_content :: Lens.Lens' PublishLayerVersion LayerVersionContentInput
publishLayerVersion_content = Lens.lens (\PublishLayerVersion' {content} -> content) (\s@PublishLayerVersion' {} a -> s {content = a} :: PublishLayerVersion)

instance Core.AWSRequest PublishLayerVersion where
  type
    AWSResponse PublishLayerVersion =
      PublishLayerVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishLayerVersionResponse'
            Core.<$> (x Core..?> "CreatedDate")
            Core.<*> (x Core..?> "LayerArn")
            Core.<*> (x Core..?> "Version")
            Core.<*> (x Core..?> "LayerVersionArn")
            Core.<*> (x Core..?> "Content")
            Core.<*> ( x Core..?> "CompatibleRuntimes"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Description")
            Core.<*> (x Core..?> "LicenseInfo")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PublishLayerVersion

instance Core.NFData PublishLayerVersion

instance Core.ToHeaders PublishLayerVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PublishLayerVersion where
  toJSON PublishLayerVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CompatibleRuntimes" Core..=)
              Core.<$> compatibleRuntimes,
            ("Description" Core..=) Core.<$> description,
            ("LicenseInfo" Core..=) Core.<$> licenseInfo,
            Core.Just ("Content" Core..= content)
          ]
      )

instance Core.ToPath PublishLayerVersion where
  toPath PublishLayerVersion' {..} =
    Core.mconcat
      [ "/2018-10-31/layers/",
        Core.toBS layerName,
        "/versions"
      ]

instance Core.ToQuery PublishLayerVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPublishLayerVersionResponse' smart constructor.
data PublishLayerVersionResponse = PublishLayerVersionResponse'
  { -- | The date that the layer version was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Core.Maybe Core.Text,
    -- | The ARN of the layer.
    layerArn :: Core.Maybe Core.Text,
    -- | The version number.
    version :: Core.Maybe Core.Integer,
    -- | The ARN of the layer version.
    layerVersionArn :: Core.Maybe Core.Text,
    -- | Details about the layer version.
    content :: Core.Maybe LayerVersionContentOutput,
    -- | The layer\'s compatible runtimes.
    compatibleRuntimes :: Core.Maybe [Runtime],
    -- | The description of the version.
    description :: Core.Maybe Core.Text,
    -- | The layer\'s software license.
    licenseInfo :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PublishLayerVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'publishLayerVersionResponse_createdDate' - The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'layerArn', 'publishLayerVersionResponse_layerArn' - The ARN of the layer.
--
-- 'version', 'publishLayerVersionResponse_version' - The version number.
--
-- 'layerVersionArn', 'publishLayerVersionResponse_layerVersionArn' - The ARN of the layer version.
--
-- 'content', 'publishLayerVersionResponse_content' - Details about the layer version.
--
-- 'compatibleRuntimes', 'publishLayerVersionResponse_compatibleRuntimes' - The layer\'s compatible runtimes.
--
-- 'description', 'publishLayerVersionResponse_description' - The description of the version.
--
-- 'licenseInfo', 'publishLayerVersionResponse_licenseInfo' - The layer\'s software license.
--
-- 'httpStatus', 'publishLayerVersionResponse_httpStatus' - The response's http status code.
newPublishLayerVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PublishLayerVersionResponse
newPublishLayerVersionResponse pHttpStatus_ =
  PublishLayerVersionResponse'
    { createdDate =
        Core.Nothing,
      layerArn = Core.Nothing,
      version = Core.Nothing,
      layerVersionArn = Core.Nothing,
      content = Core.Nothing,
      compatibleRuntimes = Core.Nothing,
      description = Core.Nothing,
      licenseInfo = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
publishLayerVersionResponse_createdDate :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Text)
publishLayerVersionResponse_createdDate = Lens.lens (\PublishLayerVersionResponse' {createdDate} -> createdDate) (\s@PublishLayerVersionResponse' {} a -> s {createdDate = a} :: PublishLayerVersionResponse)

-- | The ARN of the layer.
publishLayerVersionResponse_layerArn :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Text)
publishLayerVersionResponse_layerArn = Lens.lens (\PublishLayerVersionResponse' {layerArn} -> layerArn) (\s@PublishLayerVersionResponse' {} a -> s {layerArn = a} :: PublishLayerVersionResponse)

-- | The version number.
publishLayerVersionResponse_version :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Integer)
publishLayerVersionResponse_version = Lens.lens (\PublishLayerVersionResponse' {version} -> version) (\s@PublishLayerVersionResponse' {} a -> s {version = a} :: PublishLayerVersionResponse)

-- | The ARN of the layer version.
publishLayerVersionResponse_layerVersionArn :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Text)
publishLayerVersionResponse_layerVersionArn = Lens.lens (\PublishLayerVersionResponse' {layerVersionArn} -> layerVersionArn) (\s@PublishLayerVersionResponse' {} a -> s {layerVersionArn = a} :: PublishLayerVersionResponse)

-- | Details about the layer version.
publishLayerVersionResponse_content :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe LayerVersionContentOutput)
publishLayerVersionResponse_content = Lens.lens (\PublishLayerVersionResponse' {content} -> content) (\s@PublishLayerVersionResponse' {} a -> s {content = a} :: PublishLayerVersionResponse)

-- | The layer\'s compatible runtimes.
publishLayerVersionResponse_compatibleRuntimes :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe [Runtime])
publishLayerVersionResponse_compatibleRuntimes = Lens.lens (\PublishLayerVersionResponse' {compatibleRuntimes} -> compatibleRuntimes) (\s@PublishLayerVersionResponse' {} a -> s {compatibleRuntimes = a} :: PublishLayerVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | The description of the version.
publishLayerVersionResponse_description :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Text)
publishLayerVersionResponse_description = Lens.lens (\PublishLayerVersionResponse' {description} -> description) (\s@PublishLayerVersionResponse' {} a -> s {description = a} :: PublishLayerVersionResponse)

-- | The layer\'s software license.
publishLayerVersionResponse_licenseInfo :: Lens.Lens' PublishLayerVersionResponse (Core.Maybe Core.Text)
publishLayerVersionResponse_licenseInfo = Lens.lens (\PublishLayerVersionResponse' {licenseInfo} -> licenseInfo) (\s@PublishLayerVersionResponse' {} a -> s {licenseInfo = a} :: PublishLayerVersionResponse)

-- | The response's http status code.
publishLayerVersionResponse_httpStatus :: Lens.Lens' PublishLayerVersionResponse Core.Int
publishLayerVersionResponse_httpStatus = Lens.lens (\PublishLayerVersionResponse' {httpStatus} -> httpStatus) (\s@PublishLayerVersionResponse' {} a -> s {httpStatus = a} :: PublishLayerVersionResponse)

instance Core.NFData PublishLayerVersionResponse
