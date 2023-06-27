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
-- Module      : Amazonka.Lambda.PublishLayerVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html Lambda layer>
-- from a ZIP archive. Each time you call @PublishLayerVersion@ with the
-- same layer name, a new version is created.
--
-- Add layers to your function with CreateFunction or
-- UpdateFunctionConfiguration.
module Amazonka.Lambda.PublishLayerVersion
  ( -- * Creating a Request
    PublishLayerVersion (..),
    newPublishLayerVersion,

    -- * Request Lenses
    publishLayerVersion_compatibleArchitectures,
    publishLayerVersion_compatibleRuntimes,
    publishLayerVersion_description,
    publishLayerVersion_licenseInfo,
    publishLayerVersion_layerName,
    publishLayerVersion_content,

    -- * Destructuring the Response
    PublishLayerVersionResponse (..),
    newPublishLayerVersionResponse,

    -- * Response Lenses
    publishLayerVersionResponse_compatibleArchitectures,
    publishLayerVersionResponse_compatibleRuntimes,
    publishLayerVersionResponse_content,
    publishLayerVersionResponse_createdDate,
    publishLayerVersionResponse_description,
    publishLayerVersionResponse_layerArn,
    publishLayerVersionResponse_layerVersionArn,
    publishLayerVersionResponse_licenseInfo,
    publishLayerVersionResponse_version,
    publishLayerVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPublishLayerVersion' smart constructor.
data PublishLayerVersion = PublishLayerVersion'
  { -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
    compatibleArchitectures :: Prelude.Maybe [Architecture],
    -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes>.
    -- Used for filtering with ListLayers and ListLayerVersions.
    --
    -- The following list includes deprecated runtimes. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
    compatibleRuntimes :: Prelude.Maybe [Runtime],
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s software license. It can be any of the following:
    --
    -- -   An <https://spdx.org/licenses/ SPDX license identifier>. For
    --     example, @MIT@.
    --
    -- -   The URL of a license hosted on the internet. For example,
    --     @https:\/\/opensource.org\/licenses\/MIT@.
    --
    -- -   The full text of the license.
    licenseInfo :: Prelude.Maybe Prelude.Text,
    -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Prelude.Text,
    -- | The function layer archive.
    content :: LayerVersionContentInput
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishLayerVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleArchitectures', 'publishLayerVersion_compatibleArchitectures' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
--
-- 'compatibleRuntimes', 'publishLayerVersion_compatibleRuntimes' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes>.
-- Used for filtering with ListLayers and ListLayerVersions.
--
-- The following list includes deprecated runtimes. For more information,
-- see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
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
  Prelude.Text ->
  -- | 'content'
  LayerVersionContentInput ->
  PublishLayerVersion
newPublishLayerVersion pLayerName_ pContent_ =
  PublishLayerVersion'
    { compatibleArchitectures =
        Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing,
      description = Prelude.Nothing,
      licenseInfo = Prelude.Nothing,
      layerName = pLayerName_,
      content = pContent_
    }

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
publishLayerVersion_compatibleArchitectures :: Lens.Lens' PublishLayerVersion (Prelude.Maybe [Architecture])
publishLayerVersion_compatibleArchitectures = Lens.lens (\PublishLayerVersion' {compatibleArchitectures} -> compatibleArchitectures) (\s@PublishLayerVersion' {} a -> s {compatibleArchitectures = a} :: PublishLayerVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes>.
-- Used for filtering with ListLayers and ListLayerVersions.
--
-- The following list includes deprecated runtimes. For more information,
-- see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
publishLayerVersion_compatibleRuntimes :: Lens.Lens' PublishLayerVersion (Prelude.Maybe [Runtime])
publishLayerVersion_compatibleRuntimes = Lens.lens (\PublishLayerVersion' {compatibleRuntimes} -> compatibleRuntimes) (\s@PublishLayerVersion' {} a -> s {compatibleRuntimes = a} :: PublishLayerVersion) Prelude.. Lens.mapping Lens.coerced

-- | The description of the version.
publishLayerVersion_description :: Lens.Lens' PublishLayerVersion (Prelude.Maybe Prelude.Text)
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
publishLayerVersion_licenseInfo :: Lens.Lens' PublishLayerVersion (Prelude.Maybe Prelude.Text)
publishLayerVersion_licenseInfo = Lens.lens (\PublishLayerVersion' {licenseInfo} -> licenseInfo) (\s@PublishLayerVersion' {} a -> s {licenseInfo = a} :: PublishLayerVersion)

-- | The name or Amazon Resource Name (ARN) of the layer.
publishLayerVersion_layerName :: Lens.Lens' PublishLayerVersion Prelude.Text
publishLayerVersion_layerName = Lens.lens (\PublishLayerVersion' {layerName} -> layerName) (\s@PublishLayerVersion' {} a -> s {layerName = a} :: PublishLayerVersion)

-- | The function layer archive.
publishLayerVersion_content :: Lens.Lens' PublishLayerVersion LayerVersionContentInput
publishLayerVersion_content = Lens.lens (\PublishLayerVersion' {content} -> content) (\s@PublishLayerVersion' {} a -> s {content = a} :: PublishLayerVersion)

instance Core.AWSRequest PublishLayerVersion where
  type
    AWSResponse PublishLayerVersion =
      PublishLayerVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PublishLayerVersionResponse'
            Prelude.<$> ( x
                            Data..?> "CompatibleArchitectures"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "CompatibleRuntimes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Content")
            Prelude.<*> (x Data..?> "CreatedDate")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LayerArn")
            Prelude.<*> (x Data..?> "LayerVersionArn")
            Prelude.<*> (x Data..?> "LicenseInfo")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PublishLayerVersion where
  hashWithSalt _salt PublishLayerVersion' {..} =
    _salt
      `Prelude.hashWithSalt` compatibleArchitectures
      `Prelude.hashWithSalt` compatibleRuntimes
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` licenseInfo
      `Prelude.hashWithSalt` layerName
      `Prelude.hashWithSalt` content

instance Prelude.NFData PublishLayerVersion where
  rnf PublishLayerVersion' {..} =
    Prelude.rnf compatibleArchitectures
      `Prelude.seq` Prelude.rnf compatibleRuntimes
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf licenseInfo
      `Prelude.seq` Prelude.rnf layerName
      `Prelude.seq` Prelude.rnf content

instance Data.ToHeaders PublishLayerVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PublishLayerVersion where
  toJSON PublishLayerVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompatibleArchitectures" Data..=)
              Prelude.<$> compatibleArchitectures,
            ("CompatibleRuntimes" Data..=)
              Prelude.<$> compatibleRuntimes,
            ("Description" Data..=) Prelude.<$> description,
            ("LicenseInfo" Data..=) Prelude.<$> licenseInfo,
            Prelude.Just ("Content" Data..= content)
          ]
      )

instance Data.ToPath PublishLayerVersion where
  toPath PublishLayerVersion' {..} =
    Prelude.mconcat
      [ "/2018-10-31/layers/",
        Data.toBS layerName,
        "/versions"
      ]

instance Data.ToQuery PublishLayerVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPublishLayerVersionResponse' smart constructor.
data PublishLayerVersionResponse = PublishLayerVersionResponse'
  { -- | A list of compatible
    -- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
    compatibleArchitectures :: Prelude.Maybe [Architecture],
    -- | The layer\'s compatible runtimes.
    --
    -- The following list includes deprecated runtimes. For more information,
    -- see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
    compatibleRuntimes :: Prelude.Maybe [Runtime],
    -- | Details about the layer version.
    content :: Prelude.Maybe LayerVersionContentOutput,
    -- | The date that the layer version was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The description of the version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the layer.
    layerArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the layer version.
    layerVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The layer\'s software license.
    licenseInfo :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PublishLayerVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleArchitectures', 'publishLayerVersionResponse_compatibleArchitectures' - A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
--
-- 'compatibleRuntimes', 'publishLayerVersionResponse_compatibleRuntimes' - The layer\'s compatible runtimes.
--
-- The following list includes deprecated runtimes. For more information,
-- see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
--
-- 'content', 'publishLayerVersionResponse_content' - Details about the layer version.
--
-- 'createdDate', 'publishLayerVersionResponse_createdDate' - The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'description', 'publishLayerVersionResponse_description' - The description of the version.
--
-- 'layerArn', 'publishLayerVersionResponse_layerArn' - The ARN of the layer.
--
-- 'layerVersionArn', 'publishLayerVersionResponse_layerVersionArn' - The ARN of the layer version.
--
-- 'licenseInfo', 'publishLayerVersionResponse_licenseInfo' - The layer\'s software license.
--
-- 'version', 'publishLayerVersionResponse_version' - The version number.
--
-- 'httpStatus', 'publishLayerVersionResponse_httpStatus' - The response's http status code.
newPublishLayerVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PublishLayerVersionResponse
newPublishLayerVersionResponse pHttpStatus_ =
  PublishLayerVersionResponse'
    { compatibleArchitectures =
        Prelude.Nothing,
      compatibleRuntimes = Prelude.Nothing,
      content = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      layerArn = Prelude.Nothing,
      layerVersionArn = Prelude.Nothing,
      licenseInfo = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of compatible
-- <https://docs.aws.amazon.com/lambda/latest/dg/foundation-arch.html instruction set architectures>.
publishLayerVersionResponse_compatibleArchitectures :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe [Architecture])
publishLayerVersionResponse_compatibleArchitectures = Lens.lens (\PublishLayerVersionResponse' {compatibleArchitectures} -> compatibleArchitectures) (\s@PublishLayerVersionResponse' {} a -> s {compatibleArchitectures = a} :: PublishLayerVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The layer\'s compatible runtimes.
--
-- The following list includes deprecated runtimes. For more information,
-- see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html#runtime-support-policy Runtime deprecation policy>.
publishLayerVersionResponse_compatibleRuntimes :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe [Runtime])
publishLayerVersionResponse_compatibleRuntimes = Lens.lens (\PublishLayerVersionResponse' {compatibleRuntimes} -> compatibleRuntimes) (\s@PublishLayerVersionResponse' {} a -> s {compatibleRuntimes = a} :: PublishLayerVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | Details about the layer version.
publishLayerVersionResponse_content :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe LayerVersionContentOutput)
publishLayerVersionResponse_content = Lens.lens (\PublishLayerVersionResponse' {content} -> content) (\s@PublishLayerVersionResponse' {} a -> s {content = a} :: PublishLayerVersionResponse)

-- | The date that the layer version was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
publishLayerVersionResponse_createdDate :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe Prelude.Text)
publishLayerVersionResponse_createdDate = Lens.lens (\PublishLayerVersionResponse' {createdDate} -> createdDate) (\s@PublishLayerVersionResponse' {} a -> s {createdDate = a} :: PublishLayerVersionResponse)

-- | The description of the version.
publishLayerVersionResponse_description :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe Prelude.Text)
publishLayerVersionResponse_description = Lens.lens (\PublishLayerVersionResponse' {description} -> description) (\s@PublishLayerVersionResponse' {} a -> s {description = a} :: PublishLayerVersionResponse)

-- | The ARN of the layer.
publishLayerVersionResponse_layerArn :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe Prelude.Text)
publishLayerVersionResponse_layerArn = Lens.lens (\PublishLayerVersionResponse' {layerArn} -> layerArn) (\s@PublishLayerVersionResponse' {} a -> s {layerArn = a} :: PublishLayerVersionResponse)

-- | The ARN of the layer version.
publishLayerVersionResponse_layerVersionArn :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe Prelude.Text)
publishLayerVersionResponse_layerVersionArn = Lens.lens (\PublishLayerVersionResponse' {layerVersionArn} -> layerVersionArn) (\s@PublishLayerVersionResponse' {} a -> s {layerVersionArn = a} :: PublishLayerVersionResponse)

-- | The layer\'s software license.
publishLayerVersionResponse_licenseInfo :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe Prelude.Text)
publishLayerVersionResponse_licenseInfo = Lens.lens (\PublishLayerVersionResponse' {licenseInfo} -> licenseInfo) (\s@PublishLayerVersionResponse' {} a -> s {licenseInfo = a} :: PublishLayerVersionResponse)

-- | The version number.
publishLayerVersionResponse_version :: Lens.Lens' PublishLayerVersionResponse (Prelude.Maybe Prelude.Integer)
publishLayerVersionResponse_version = Lens.lens (\PublishLayerVersionResponse' {version} -> version) (\s@PublishLayerVersionResponse' {} a -> s {version = a} :: PublishLayerVersionResponse)

-- | The response's http status code.
publishLayerVersionResponse_httpStatus :: Lens.Lens' PublishLayerVersionResponse Prelude.Int
publishLayerVersionResponse_httpStatus = Lens.lens (\PublishLayerVersionResponse' {httpStatus} -> httpStatus) (\s@PublishLayerVersionResponse' {} a -> s {httpStatus = a} :: PublishLayerVersionResponse)

instance Prelude.NFData PublishLayerVersionResponse where
  rnf PublishLayerVersionResponse' {..} =
    Prelude.rnf compatibleArchitectures
      `Prelude.seq` Prelude.rnf compatibleRuntimes
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf layerArn
      `Prelude.seq` Prelude.rnf layerVersionArn
      `Prelude.seq` Prelude.rnf licenseInfo
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
