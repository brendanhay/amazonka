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
-- Module      : Amazonka.MediaTailor.CreateLiveSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates name for a specific live source in a source location.
module Amazonka.MediaTailor.CreateLiveSource
  ( -- * Creating a Request
    CreateLiveSource (..),
    newCreateLiveSource,

    -- * Request Lenses
    createLiveSource_tags,
    createLiveSource_sourceLocationName,
    createLiveSource_liveSourceName,
    createLiveSource_httpPackageConfigurations,

    -- * Destructuring the Response
    CreateLiveSourceResponse (..),
    newCreateLiveSourceResponse,

    -- * Response Lenses
    createLiveSourceResponse_tags,
    createLiveSourceResponse_liveSourceName,
    createLiveSourceResponse_arn,
    createLiveSourceResponse_lastModifiedTime,
    createLiveSourceResponse_creationTime,
    createLiveSourceResponse_sourceLocationName,
    createLiveSourceResponse_httpPackageConfigurations,
    createLiveSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLiveSource' smart constructor.
data CreateLiveSource = CreateLiveSource'
  { -- | The tags to assign to the live source.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The identifier for the source location you are working on.
    sourceLocationName :: Prelude.Text,
    -- | The identifier for the live source you are working on.
    liveSourceName :: Prelude.Text,
    -- | A list of HTTP package configuration parameters for this live source.
    httpPackageConfigurations :: [HttpPackageConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLiveSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLiveSource_tags' - The tags to assign to the live source.
--
-- 'sourceLocationName', 'createLiveSource_sourceLocationName' - The identifier for the source location you are working on.
--
-- 'liveSourceName', 'createLiveSource_liveSourceName' - The identifier for the live source you are working on.
--
-- 'httpPackageConfigurations', 'createLiveSource_httpPackageConfigurations' - A list of HTTP package configuration parameters for this live source.
newCreateLiveSource ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'liveSourceName'
  Prelude.Text ->
  CreateLiveSource
newCreateLiveSource
  pSourceLocationName_
  pLiveSourceName_ =
    CreateLiveSource'
      { tags = Prelude.Nothing,
        sourceLocationName = pSourceLocationName_,
        liveSourceName = pLiveSourceName_,
        httpPackageConfigurations = Prelude.mempty
      }

-- | The tags to assign to the live source.
createLiveSource_tags :: Lens.Lens' CreateLiveSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLiveSource_tags = Lens.lens (\CreateLiveSource' {tags} -> tags) (\s@CreateLiveSource' {} a -> s {tags = a} :: CreateLiveSource) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the source location you are working on.
createLiveSource_sourceLocationName :: Lens.Lens' CreateLiveSource Prelude.Text
createLiveSource_sourceLocationName = Lens.lens (\CreateLiveSource' {sourceLocationName} -> sourceLocationName) (\s@CreateLiveSource' {} a -> s {sourceLocationName = a} :: CreateLiveSource)

-- | The identifier for the live source you are working on.
createLiveSource_liveSourceName :: Lens.Lens' CreateLiveSource Prelude.Text
createLiveSource_liveSourceName = Lens.lens (\CreateLiveSource' {liveSourceName} -> liveSourceName) (\s@CreateLiveSource' {} a -> s {liveSourceName = a} :: CreateLiveSource)

-- | A list of HTTP package configuration parameters for this live source.
createLiveSource_httpPackageConfigurations :: Lens.Lens' CreateLiveSource [HttpPackageConfiguration]
createLiveSource_httpPackageConfigurations = Lens.lens (\CreateLiveSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@CreateLiveSource' {} a -> s {httpPackageConfigurations = a} :: CreateLiveSource) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLiveSource where
  type
    AWSResponse CreateLiveSource =
      CreateLiveSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLiveSourceResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LiveSourceName")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "SourceLocationName")
            Prelude.<*> ( x Core..?> "HttpPackageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLiveSource where
  hashWithSalt _salt CreateLiveSource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` httpPackageConfigurations

instance Prelude.NFData CreateLiveSource where
  rnf CreateLiveSource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations

instance Core.ToHeaders CreateLiveSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLiveSource where
  toJSON CreateLiveSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "HttpPackageConfigurations"
                  Core..= httpPackageConfigurations
              )
          ]
      )

instance Core.ToPath CreateLiveSource where
  toPath CreateLiveSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Core.toBS sourceLocationName,
        "/liveSource/",
        Core.toBS liveSourceName
      ]

instance Core.ToQuery CreateLiveSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLiveSourceResponse' smart constructor.
data CreateLiveSourceResponse = CreateLiveSourceResponse'
  { -- | The tags assigned to the live source.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the live source.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the live source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that indicates when the live source was modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp that indicates when the live source was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the source location associated with the VOD source.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The HTTP package configurations.
    httpPackageConfigurations :: Prelude.Maybe [HttpPackageConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLiveSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLiveSourceResponse_tags' - The tags assigned to the live source.
--
-- 'liveSourceName', 'createLiveSourceResponse_liveSourceName' - The name of the live source.
--
-- 'arn', 'createLiveSourceResponse_arn' - The ARN of the live source.
--
-- 'lastModifiedTime', 'createLiveSourceResponse_lastModifiedTime' - The timestamp that indicates when the live source was modified.
--
-- 'creationTime', 'createLiveSourceResponse_creationTime' - The timestamp that indicates when the live source was created.
--
-- 'sourceLocationName', 'createLiveSourceResponse_sourceLocationName' - The name of the source location associated with the VOD source.
--
-- 'httpPackageConfigurations', 'createLiveSourceResponse_httpPackageConfigurations' - The HTTP package configurations.
--
-- 'httpStatus', 'createLiveSourceResponse_httpStatus' - The response's http status code.
newCreateLiveSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLiveSourceResponse
newCreateLiveSourceResponse pHttpStatus_ =
  CreateLiveSourceResponse'
    { tags = Prelude.Nothing,
      liveSourceName = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      httpPackageConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags assigned to the live source.
createLiveSourceResponse_tags :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLiveSourceResponse_tags = Lens.lens (\CreateLiveSourceResponse' {tags} -> tags) (\s@CreateLiveSourceResponse' {} a -> s {tags = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the live source.
createLiveSourceResponse_liveSourceName :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.Text)
createLiveSourceResponse_liveSourceName = Lens.lens (\CreateLiveSourceResponse' {liveSourceName} -> liveSourceName) (\s@CreateLiveSourceResponse' {} a -> s {liveSourceName = a} :: CreateLiveSourceResponse)

-- | The ARN of the live source.
createLiveSourceResponse_arn :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.Text)
createLiveSourceResponse_arn = Lens.lens (\CreateLiveSourceResponse' {arn} -> arn) (\s@CreateLiveSourceResponse' {} a -> s {arn = a} :: CreateLiveSourceResponse)

-- | The timestamp that indicates when the live source was modified.
createLiveSourceResponse_lastModifiedTime :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.UTCTime)
createLiveSourceResponse_lastModifiedTime = Lens.lens (\CreateLiveSourceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateLiveSourceResponse' {} a -> s {lastModifiedTime = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp that indicates when the live source was created.
createLiveSourceResponse_creationTime :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.UTCTime)
createLiveSourceResponse_creationTime = Lens.lens (\CreateLiveSourceResponse' {creationTime} -> creationTime) (\s@CreateLiveSourceResponse' {} a -> s {creationTime = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the source location associated with the VOD source.
createLiveSourceResponse_sourceLocationName :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.Text)
createLiveSourceResponse_sourceLocationName = Lens.lens (\CreateLiveSourceResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateLiveSourceResponse' {} a -> s {sourceLocationName = a} :: CreateLiveSourceResponse)

-- | The HTTP package configurations.
createLiveSourceResponse_httpPackageConfigurations :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe [HttpPackageConfiguration])
createLiveSourceResponse_httpPackageConfigurations = Lens.lens (\CreateLiveSourceResponse' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@CreateLiveSourceResponse' {} a -> s {httpPackageConfigurations = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createLiveSourceResponse_httpStatus :: Lens.Lens' CreateLiveSourceResponse Prelude.Int
createLiveSourceResponse_httpStatus = Lens.lens (\CreateLiveSourceResponse' {httpStatus} -> httpStatus) (\s@CreateLiveSourceResponse' {} a -> s {httpStatus = a} :: CreateLiveSourceResponse)

instance Prelude.NFData CreateLiveSourceResponse where
  rnf CreateLiveSourceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
