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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The live source configuration.
module Amazonka.MediaTailor.CreateLiveSource
  ( -- * Creating a Request
    CreateLiveSource (..),
    newCreateLiveSource,

    -- * Request Lenses
    createLiveSource_tags,
    createLiveSource_httpPackageConfigurations,
    createLiveSource_liveSourceName,
    createLiveSource_sourceLocationName,

    -- * Destructuring the Response
    CreateLiveSourceResponse (..),
    newCreateLiveSourceResponse,

    -- * Response Lenses
    createLiveSourceResponse_arn,
    createLiveSourceResponse_creationTime,
    createLiveSourceResponse_httpPackageConfigurations,
    createLiveSourceResponse_lastModifiedTime,
    createLiveSourceResponse_liveSourceName,
    createLiveSourceResponse_sourceLocationName,
    createLiveSourceResponse_tags,
    createLiveSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLiveSource' smart constructor.
data CreateLiveSource = CreateLiveSource'
  { -- | The tags to assign to the live source. Tags are key-value pairs that you
    -- can associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of HTTP package configuration parameters for this live source.
    httpPackageConfigurations :: [HttpPackageConfiguration],
    -- | The name of the live source.
    liveSourceName :: Prelude.Text,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text
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
-- 'tags', 'createLiveSource_tags' - The tags to assign to the live source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpPackageConfigurations', 'createLiveSource_httpPackageConfigurations' - A list of HTTP package configuration parameters for this live source.
--
-- 'liveSourceName', 'createLiveSource_liveSourceName' - The name of the live source.
--
-- 'sourceLocationName', 'createLiveSource_sourceLocationName' - The name of the source location.
newCreateLiveSource ::
  -- | 'liveSourceName'
  Prelude.Text ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  CreateLiveSource
newCreateLiveSource
  pLiveSourceName_
  pSourceLocationName_ =
    CreateLiveSource'
      { tags = Prelude.Nothing,
        httpPackageConfigurations = Prelude.mempty,
        liveSourceName = pLiveSourceName_,
        sourceLocationName = pSourceLocationName_
      }

-- | The tags to assign to the live source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
createLiveSource_tags :: Lens.Lens' CreateLiveSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLiveSource_tags = Lens.lens (\CreateLiveSource' {tags} -> tags) (\s@CreateLiveSource' {} a -> s {tags = a} :: CreateLiveSource) Prelude.. Lens.mapping Lens.coerced

-- | A list of HTTP package configuration parameters for this live source.
createLiveSource_httpPackageConfigurations :: Lens.Lens' CreateLiveSource [HttpPackageConfiguration]
createLiveSource_httpPackageConfigurations = Lens.lens (\CreateLiveSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@CreateLiveSource' {} a -> s {httpPackageConfigurations = a} :: CreateLiveSource) Prelude.. Lens.coerced

-- | The name of the live source.
createLiveSource_liveSourceName :: Lens.Lens' CreateLiveSource Prelude.Text
createLiveSource_liveSourceName = Lens.lens (\CreateLiveSource' {liveSourceName} -> liveSourceName) (\s@CreateLiveSource' {} a -> s {liveSourceName = a} :: CreateLiveSource)

-- | The name of the source location.
createLiveSource_sourceLocationName :: Lens.Lens' CreateLiveSource Prelude.Text
createLiveSource_sourceLocationName = Lens.lens (\CreateLiveSource' {sourceLocationName} -> sourceLocationName) (\s@CreateLiveSource' {} a -> s {sourceLocationName = a} :: CreateLiveSource)

instance Core.AWSRequest CreateLiveSource where
  type
    AWSResponse CreateLiveSource =
      CreateLiveSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLiveSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> ( x Data..?> "HttpPackageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "LiveSourceName")
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLiveSource where
  hashWithSalt _salt CreateLiveSource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` httpPackageConfigurations
      `Prelude.hashWithSalt` liveSourceName
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData CreateLiveSource where
  rnf CreateLiveSource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders CreateLiveSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLiveSource where
  toJSON CreateLiveSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "HttpPackageConfigurations"
                  Data..= httpPackageConfigurations
              )
          ]
      )

instance Data.ToPath CreateLiveSource where
  toPath CreateLiveSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/liveSource/",
        Data.toBS liveSourceName
      ]

instance Data.ToQuery CreateLiveSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLiveSourceResponse' smart constructor.
data CreateLiveSourceResponse = CreateLiveSourceResponse'
  { -- | The ARN to assign to the live source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the live source was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A list of HTTP package configuration parameters for this live source.
    httpPackageConfigurations :: Prelude.Maybe [HttpPackageConfiguration],
    -- | The time the live source was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name to assign to the live source.
    liveSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name to assign to the source location of the live source.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the live source. Tags are key-value pairs that you
    -- can associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'arn', 'createLiveSourceResponse_arn' - The ARN to assign to the live source.
--
-- 'creationTime', 'createLiveSourceResponse_creationTime' - The time the live source was created.
--
-- 'httpPackageConfigurations', 'createLiveSourceResponse_httpPackageConfigurations' - A list of HTTP package configuration parameters for this live source.
--
-- 'lastModifiedTime', 'createLiveSourceResponse_lastModifiedTime' - The time the live source was last modified.
--
-- 'liveSourceName', 'createLiveSourceResponse_liveSourceName' - The name to assign to the live source.
--
-- 'sourceLocationName', 'createLiveSourceResponse_sourceLocationName' - The name to assign to the source location of the live source.
--
-- 'tags', 'createLiveSourceResponse_tags' - The tags to assign to the live source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpStatus', 'createLiveSourceResponse_httpStatus' - The response's http status code.
newCreateLiveSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLiveSourceResponse
newCreateLiveSourceResponse pHttpStatus_ =
  CreateLiveSourceResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpPackageConfigurations = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      liveSourceName = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN to assign to the live source.
createLiveSourceResponse_arn :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.Text)
createLiveSourceResponse_arn = Lens.lens (\CreateLiveSourceResponse' {arn} -> arn) (\s@CreateLiveSourceResponse' {} a -> s {arn = a} :: CreateLiveSourceResponse)

-- | The time the live source was created.
createLiveSourceResponse_creationTime :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.UTCTime)
createLiveSourceResponse_creationTime = Lens.lens (\CreateLiveSourceResponse' {creationTime} -> creationTime) (\s@CreateLiveSourceResponse' {} a -> s {creationTime = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Data._Time

-- | A list of HTTP package configuration parameters for this live source.
createLiveSourceResponse_httpPackageConfigurations :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe [HttpPackageConfiguration])
createLiveSourceResponse_httpPackageConfigurations = Lens.lens (\CreateLiveSourceResponse' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@CreateLiveSourceResponse' {} a -> s {httpPackageConfigurations = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time the live source was last modified.
createLiveSourceResponse_lastModifiedTime :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.UTCTime)
createLiveSourceResponse_lastModifiedTime = Lens.lens (\CreateLiveSourceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateLiveSourceResponse' {} a -> s {lastModifiedTime = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The name to assign to the live source.
createLiveSourceResponse_liveSourceName :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.Text)
createLiveSourceResponse_liveSourceName = Lens.lens (\CreateLiveSourceResponse' {liveSourceName} -> liveSourceName) (\s@CreateLiveSourceResponse' {} a -> s {liveSourceName = a} :: CreateLiveSourceResponse)

-- | The name to assign to the source location of the live source.
createLiveSourceResponse_sourceLocationName :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe Prelude.Text)
createLiveSourceResponse_sourceLocationName = Lens.lens (\CreateLiveSourceResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateLiveSourceResponse' {} a -> s {sourceLocationName = a} :: CreateLiveSourceResponse)

-- | The tags to assign to the live source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
createLiveSourceResponse_tags :: Lens.Lens' CreateLiveSourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLiveSourceResponse_tags = Lens.lens (\CreateLiveSourceResponse' {tags} -> tags) (\s@CreateLiveSourceResponse' {} a -> s {tags = a} :: CreateLiveSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createLiveSourceResponse_httpStatus :: Lens.Lens' CreateLiveSourceResponse Prelude.Int
createLiveSourceResponse_httpStatus = Lens.lens (\CreateLiveSourceResponse' {httpStatus} -> httpStatus) (\s@CreateLiveSourceResponse' {} a -> s {httpStatus = a} :: CreateLiveSourceResponse)

instance Prelude.NFData CreateLiveSourceResponse where
  rnf CreateLiveSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf liveSourceName
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
