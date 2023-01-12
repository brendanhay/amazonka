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
-- Module      : Amazonka.MediaTailor.UpdateVodSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a VOD source\'s configuration.
module Amazonka.MediaTailor.UpdateVodSource
  ( -- * Creating a Request
    UpdateVodSource (..),
    newUpdateVodSource,

    -- * Request Lenses
    updateVodSource_httpPackageConfigurations,
    updateVodSource_sourceLocationName,
    updateVodSource_vodSourceName,

    -- * Destructuring the Response
    UpdateVodSourceResponse (..),
    newUpdateVodSourceResponse,

    -- * Response Lenses
    updateVodSourceResponse_arn,
    updateVodSourceResponse_creationTime,
    updateVodSourceResponse_httpPackageConfigurations,
    updateVodSourceResponse_lastModifiedTime,
    updateVodSourceResponse_sourceLocationName,
    updateVodSourceResponse_tags,
    updateVodSourceResponse_vodSourceName,
    updateVodSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVodSource' smart constructor.
data UpdateVodSource = UpdateVodSource'
  { -- | A list of HTTP package configurations for the VOD source on this
    -- account.
    httpPackageConfigurations :: [HttpPackageConfiguration],
    -- | The name of the source location associated with this VOD Source.
    sourceLocationName :: Prelude.Text,
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVodSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpPackageConfigurations', 'updateVodSource_httpPackageConfigurations' - A list of HTTP package configurations for the VOD source on this
-- account.
--
-- 'sourceLocationName', 'updateVodSource_sourceLocationName' - The name of the source location associated with this VOD Source.
--
-- 'vodSourceName', 'updateVodSource_vodSourceName' - The name of the VOD source.
newUpdateVodSource ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'vodSourceName'
  Prelude.Text ->
  UpdateVodSource
newUpdateVodSource
  pSourceLocationName_
  pVodSourceName_ =
    UpdateVodSource'
      { httpPackageConfigurations =
          Prelude.mempty,
        sourceLocationName = pSourceLocationName_,
        vodSourceName = pVodSourceName_
      }

-- | A list of HTTP package configurations for the VOD source on this
-- account.
updateVodSource_httpPackageConfigurations :: Lens.Lens' UpdateVodSource [HttpPackageConfiguration]
updateVodSource_httpPackageConfigurations = Lens.lens (\UpdateVodSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@UpdateVodSource' {} a -> s {httpPackageConfigurations = a} :: UpdateVodSource) Prelude.. Lens.coerced

-- | The name of the source location associated with this VOD Source.
updateVodSource_sourceLocationName :: Lens.Lens' UpdateVodSource Prelude.Text
updateVodSource_sourceLocationName = Lens.lens (\UpdateVodSource' {sourceLocationName} -> sourceLocationName) (\s@UpdateVodSource' {} a -> s {sourceLocationName = a} :: UpdateVodSource)

-- | The name of the VOD source.
updateVodSource_vodSourceName :: Lens.Lens' UpdateVodSource Prelude.Text
updateVodSource_vodSourceName = Lens.lens (\UpdateVodSource' {vodSourceName} -> vodSourceName) (\s@UpdateVodSource' {} a -> s {vodSourceName = a} :: UpdateVodSource)

instance Core.AWSRequest UpdateVodSource where
  type
    AWSResponse UpdateVodSource =
      UpdateVodSourceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVodSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> ( x Data..?> "HttpPackageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VodSourceName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVodSource where
  hashWithSalt _salt UpdateVodSource' {..} =
    _salt
      `Prelude.hashWithSalt` httpPackageConfigurations
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` vodSourceName

instance Prelude.NFData UpdateVodSource where
  rnf UpdateVodSource' {..} =
    Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf vodSourceName

instance Data.ToHeaders UpdateVodSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVodSource where
  toJSON UpdateVodSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HttpPackageConfigurations"
                  Data..= httpPackageConfigurations
              )
          ]
      )

instance Data.ToPath UpdateVodSource where
  toPath UpdateVodSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Data.toBS sourceLocationName,
        "/vodSource/",
        Data.toBS vodSourceName
      ]

instance Data.ToQuery UpdateVodSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVodSourceResponse' smart constructor.
data UpdateVodSourceResponse = UpdateVodSourceResponse'
  { -- | The Amazon Resource Name (ARN) associated with the VOD source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that indicates when the VOD source was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A list of HTTP package configurations for the VOD source on this
    -- account.
    httpPackageConfigurations :: Prelude.Maybe [HttpPackageConfiguration],
    -- | The timestamp that indicates when the VOD source was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the source location associated with the VOD source.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the VOD source. Tags are key-value pairs that you
    -- can associate with Amazon resources to help with organization, access
    -- control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVodSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateVodSourceResponse_arn' - The Amazon Resource Name (ARN) associated with the VOD source.
--
-- 'creationTime', 'updateVodSourceResponse_creationTime' - The timestamp that indicates when the VOD source was created.
--
-- 'httpPackageConfigurations', 'updateVodSourceResponse_httpPackageConfigurations' - A list of HTTP package configurations for the VOD source on this
-- account.
--
-- 'lastModifiedTime', 'updateVodSourceResponse_lastModifiedTime' - The timestamp that indicates when the VOD source was last modified.
--
-- 'sourceLocationName', 'updateVodSourceResponse_sourceLocationName' - The name of the source location associated with the VOD source.
--
-- 'tags', 'updateVodSourceResponse_tags' - The tags to assign to the VOD source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'vodSourceName', 'updateVodSourceResponse_vodSourceName' - The name of the VOD source.
--
-- 'httpStatus', 'updateVodSourceResponse_httpStatus' - The response's http status code.
newUpdateVodSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVodSourceResponse
newUpdateVodSourceResponse pHttpStatus_ =
  UpdateVodSourceResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpPackageConfigurations = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      tags = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) associated with the VOD source.
updateVodSourceResponse_arn :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.Text)
updateVodSourceResponse_arn = Lens.lens (\UpdateVodSourceResponse' {arn} -> arn) (\s@UpdateVodSourceResponse' {} a -> s {arn = a} :: UpdateVodSourceResponse)

-- | The timestamp that indicates when the VOD source was created.
updateVodSourceResponse_creationTime :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.UTCTime)
updateVodSourceResponse_creationTime = Lens.lens (\UpdateVodSourceResponse' {creationTime} -> creationTime) (\s@UpdateVodSourceResponse' {} a -> s {creationTime = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Data._Time

-- | A list of HTTP package configurations for the VOD source on this
-- account.
updateVodSourceResponse_httpPackageConfigurations :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe [HttpPackageConfiguration])
updateVodSourceResponse_httpPackageConfigurations = Lens.lens (\UpdateVodSourceResponse' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@UpdateVodSourceResponse' {} a -> s {httpPackageConfigurations = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp that indicates when the VOD source was last modified.
updateVodSourceResponse_lastModifiedTime :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.UTCTime)
updateVodSourceResponse_lastModifiedTime = Lens.lens (\UpdateVodSourceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVodSourceResponse' {} a -> s {lastModifiedTime = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the source location associated with the VOD source.
updateVodSourceResponse_sourceLocationName :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.Text)
updateVodSourceResponse_sourceLocationName = Lens.lens (\UpdateVodSourceResponse' {sourceLocationName} -> sourceLocationName) (\s@UpdateVodSourceResponse' {} a -> s {sourceLocationName = a} :: UpdateVodSourceResponse)

-- | The tags to assign to the VOD source. Tags are key-value pairs that you
-- can associate with Amazon resources to help with organization, access
-- control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
updateVodSourceResponse_tags :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateVodSourceResponse_tags = Lens.lens (\UpdateVodSourceResponse' {tags} -> tags) (\s@UpdateVodSourceResponse' {} a -> s {tags = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VOD source.
updateVodSourceResponse_vodSourceName :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.Text)
updateVodSourceResponse_vodSourceName = Lens.lens (\UpdateVodSourceResponse' {vodSourceName} -> vodSourceName) (\s@UpdateVodSourceResponse' {} a -> s {vodSourceName = a} :: UpdateVodSourceResponse)

-- | The response's http status code.
updateVodSourceResponse_httpStatus :: Lens.Lens' UpdateVodSourceResponse Prelude.Int
updateVodSourceResponse_httpStatus = Lens.lens (\UpdateVodSourceResponse' {httpStatus} -> httpStatus) (\s@UpdateVodSourceResponse' {} a -> s {httpStatus = a} :: UpdateVodSourceResponse)

instance Prelude.NFData UpdateVodSourceResponse where
  rnf UpdateVodSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf httpStatus
