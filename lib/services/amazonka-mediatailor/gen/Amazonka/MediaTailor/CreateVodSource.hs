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
-- Module      : Amazonka.MediaTailor.CreateVodSource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The VOD source configuration parameters.
module Amazonka.MediaTailor.CreateVodSource
  ( -- * Creating a Request
    CreateVodSource (..),
    newCreateVodSource,

    -- * Request Lenses
    createVodSource_tags,
    createVodSource_httpPackageConfigurations,
    createVodSource_sourceLocationName,
    createVodSource_vodSourceName,

    -- * Destructuring the Response
    CreateVodSourceResponse (..),
    newCreateVodSourceResponse,

    -- * Response Lenses
    createVodSourceResponse_tags,
    createVodSourceResponse_vodSourceName,
    createVodSourceResponse_arn,
    createVodSourceResponse_lastModifiedTime,
    createVodSourceResponse_creationTime,
    createVodSourceResponse_sourceLocationName,
    createVodSourceResponse_httpPackageConfigurations,
    createVodSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVodSource' smart constructor.
data CreateVodSource = CreateVodSource'
  { -- | The tags to assign to the VOD source. Tags are key-value pairs that you
    -- can associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of HTTP package configuration parameters for this VOD source.
    httpPackageConfigurations :: [HttpPackageConfiguration],
    -- | The name of the source location for this VOD source.
    sourceLocationName :: Prelude.Text,
    -- | The name associated with the VOD source.>
    vodSourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVodSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVodSource_tags' - The tags to assign to the VOD source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpPackageConfigurations', 'createVodSource_httpPackageConfigurations' - A list of HTTP package configuration parameters for this VOD source.
--
-- 'sourceLocationName', 'createVodSource_sourceLocationName' - The name of the source location for this VOD source.
--
-- 'vodSourceName', 'createVodSource_vodSourceName' - The name associated with the VOD source.>
newCreateVodSource ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'vodSourceName'
  Prelude.Text ->
  CreateVodSource
newCreateVodSource
  pSourceLocationName_
  pVodSourceName_ =
    CreateVodSource'
      { tags = Prelude.Nothing,
        httpPackageConfigurations = Prelude.mempty,
        sourceLocationName = pSourceLocationName_,
        vodSourceName = pVodSourceName_
      }

-- | The tags to assign to the VOD source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
createVodSource_tags :: Lens.Lens' CreateVodSource (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVodSource_tags = Lens.lens (\CreateVodSource' {tags} -> tags) (\s@CreateVodSource' {} a -> s {tags = a} :: CreateVodSource) Prelude.. Lens.mapping Lens.coerced

-- | A list of HTTP package configuration parameters for this VOD source.
createVodSource_httpPackageConfigurations :: Lens.Lens' CreateVodSource [HttpPackageConfiguration]
createVodSource_httpPackageConfigurations = Lens.lens (\CreateVodSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@CreateVodSource' {} a -> s {httpPackageConfigurations = a} :: CreateVodSource) Prelude.. Lens.coerced

-- | The name of the source location for this VOD source.
createVodSource_sourceLocationName :: Lens.Lens' CreateVodSource Prelude.Text
createVodSource_sourceLocationName = Lens.lens (\CreateVodSource' {sourceLocationName} -> sourceLocationName) (\s@CreateVodSource' {} a -> s {sourceLocationName = a} :: CreateVodSource)

-- | The name associated with the VOD source.>
createVodSource_vodSourceName :: Lens.Lens' CreateVodSource Prelude.Text
createVodSource_vodSourceName = Lens.lens (\CreateVodSource' {vodSourceName} -> vodSourceName) (\s@CreateVodSource' {} a -> s {vodSourceName = a} :: CreateVodSource)

instance Core.AWSRequest CreateVodSource where
  type
    AWSResponse CreateVodSource =
      CreateVodSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVodSourceResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VodSourceName")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> ( x Data..?> "HttpPackageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVodSource where
  hashWithSalt _salt CreateVodSource' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` httpPackageConfigurations
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` vodSourceName

instance Prelude.NFData CreateVodSource where
  rnf CreateVodSource' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf vodSourceName

instance Data.ToHeaders CreateVodSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVodSource where
  toJSON CreateVodSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ( "HttpPackageConfigurations"
                  Data..= httpPackageConfigurations
              )
          ]
      )

instance Data.ToPath CreateVodSource where
  toPath CreateVodSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/vodSource/",
        Data.toBS vodSourceName
      ]

instance Data.ToQuery CreateVodSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVodSourceResponse' smart constructor.
data CreateVodSourceResponse = CreateVodSourceResponse'
  { -- | The tags to assign to the VOD source. Tags are key-value pairs that you
    -- can associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name to assign to the VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The ARN to assign to this VOD source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the VOD source was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The time the VOD source was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name to assign to the source location for this VOD source.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | A list of HTTP package configuration parameters for this VOD source.
    httpPackageConfigurations :: Prelude.Maybe [HttpPackageConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVodSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createVodSourceResponse_tags' - The tags to assign to the VOD source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'vodSourceName', 'createVodSourceResponse_vodSourceName' - The name to assign to the VOD source.
--
-- 'arn', 'createVodSourceResponse_arn' - The ARN to assign to this VOD source.
--
-- 'lastModifiedTime', 'createVodSourceResponse_lastModifiedTime' - The time the VOD source was last modified.
--
-- 'creationTime', 'createVodSourceResponse_creationTime' - The time the VOD source was created.
--
-- 'sourceLocationName', 'createVodSourceResponse_sourceLocationName' - The name to assign to the source location for this VOD source.
--
-- 'httpPackageConfigurations', 'createVodSourceResponse_httpPackageConfigurations' - A list of HTTP package configuration parameters for this VOD source.
--
-- 'httpStatus', 'createVodSourceResponse_httpStatus' - The response's http status code.
newCreateVodSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVodSourceResponse
newCreateVodSourceResponse pHttpStatus_ =
  CreateVodSourceResponse'
    { tags = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      httpPackageConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags to assign to the VOD source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
createVodSourceResponse_tags :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createVodSourceResponse_tags = Lens.lens (\CreateVodSourceResponse' {tags} -> tags) (\s@CreateVodSourceResponse' {} a -> s {tags = a} :: CreateVodSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name to assign to the VOD source.
createVodSourceResponse_vodSourceName :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe Prelude.Text)
createVodSourceResponse_vodSourceName = Lens.lens (\CreateVodSourceResponse' {vodSourceName} -> vodSourceName) (\s@CreateVodSourceResponse' {} a -> s {vodSourceName = a} :: CreateVodSourceResponse)

-- | The ARN to assign to this VOD source.
createVodSourceResponse_arn :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe Prelude.Text)
createVodSourceResponse_arn = Lens.lens (\CreateVodSourceResponse' {arn} -> arn) (\s@CreateVodSourceResponse' {} a -> s {arn = a} :: CreateVodSourceResponse)

-- | The time the VOD source was last modified.
createVodSourceResponse_lastModifiedTime :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe Prelude.UTCTime)
createVodSourceResponse_lastModifiedTime = Lens.lens (\CreateVodSourceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateVodSourceResponse' {} a -> s {lastModifiedTime = a} :: CreateVodSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The time the VOD source was created.
createVodSourceResponse_creationTime :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe Prelude.UTCTime)
createVodSourceResponse_creationTime = Lens.lens (\CreateVodSourceResponse' {creationTime} -> creationTime) (\s@CreateVodSourceResponse' {} a -> s {creationTime = a} :: CreateVodSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The name to assign to the source location for this VOD source.
createVodSourceResponse_sourceLocationName :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe Prelude.Text)
createVodSourceResponse_sourceLocationName = Lens.lens (\CreateVodSourceResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateVodSourceResponse' {} a -> s {sourceLocationName = a} :: CreateVodSourceResponse)

-- | A list of HTTP package configuration parameters for this VOD source.
createVodSourceResponse_httpPackageConfigurations :: Lens.Lens' CreateVodSourceResponse (Prelude.Maybe [HttpPackageConfiguration])
createVodSourceResponse_httpPackageConfigurations = Lens.lens (\CreateVodSourceResponse' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@CreateVodSourceResponse' {} a -> s {httpPackageConfigurations = a} :: CreateVodSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createVodSourceResponse_httpStatus :: Lens.Lens' CreateVodSourceResponse Prelude.Int
createVodSourceResponse_httpStatus = Lens.lens (\CreateVodSourceResponse' {httpStatus} -> httpStatus) (\s@CreateVodSourceResponse' {} a -> s {httpStatus = a} :: CreateVodSourceResponse)

instance Prelude.NFData CreateVodSourceResponse where
  rnf CreateVodSourceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
