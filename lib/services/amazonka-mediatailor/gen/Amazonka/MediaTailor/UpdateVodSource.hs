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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specific VOD source in a specific source location.
module Amazonka.MediaTailor.UpdateVodSource
  ( -- * Creating a Request
    UpdateVodSource (..),
    newUpdateVodSource,

    -- * Request Lenses
    updateVodSource_sourceLocationName,
    updateVodSource_vodSourceName,
    updateVodSource_httpPackageConfigurations,

    -- * Destructuring the Response
    UpdateVodSourceResponse (..),
    newUpdateVodSourceResponse,

    -- * Response Lenses
    updateVodSourceResponse_creationTime,
    updateVodSourceResponse_sourceLocationName,
    updateVodSourceResponse_arn,
    updateVodSourceResponse_lastModifiedTime,
    updateVodSourceResponse_httpPackageConfigurations,
    updateVodSourceResponse_vodSourceName,
    updateVodSourceResponse_tags,
    updateVodSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVodSource' smart constructor.
data UpdateVodSource = UpdateVodSource'
  { -- | The identifier for the source location you are working on.
    sourceLocationName :: Prelude.Text,
    -- | The identifier for the VOD source you are working on.
    vodSourceName :: Prelude.Text,
    -- | An array of HTTP package configurations for the VOD source on this
    -- account.
    httpPackageConfigurations :: [HttpPackageConfiguration]
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
-- 'sourceLocationName', 'updateVodSource_sourceLocationName' - The identifier for the source location you are working on.
--
-- 'vodSourceName', 'updateVodSource_vodSourceName' - The identifier for the VOD source you are working on.
--
-- 'httpPackageConfigurations', 'updateVodSource_httpPackageConfigurations' - An array of HTTP package configurations for the VOD source on this
-- account.
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
      { sourceLocationName =
          pSourceLocationName_,
        vodSourceName = pVodSourceName_,
        httpPackageConfigurations = Prelude.mempty
      }

-- | The identifier for the source location you are working on.
updateVodSource_sourceLocationName :: Lens.Lens' UpdateVodSource Prelude.Text
updateVodSource_sourceLocationName = Lens.lens (\UpdateVodSource' {sourceLocationName} -> sourceLocationName) (\s@UpdateVodSource' {} a -> s {sourceLocationName = a} :: UpdateVodSource)

-- | The identifier for the VOD source you are working on.
updateVodSource_vodSourceName :: Lens.Lens' UpdateVodSource Prelude.Text
updateVodSource_vodSourceName = Lens.lens (\UpdateVodSource' {vodSourceName} -> vodSourceName) (\s@UpdateVodSource' {} a -> s {vodSourceName = a} :: UpdateVodSource)

-- | An array of HTTP package configurations for the VOD source on this
-- account.
updateVodSource_httpPackageConfigurations :: Lens.Lens' UpdateVodSource [HttpPackageConfiguration]
updateVodSource_httpPackageConfigurations = Lens.lens (\UpdateVodSource' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@UpdateVodSource' {} a -> s {httpPackageConfigurations = a} :: UpdateVodSource) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateVodSource where
  type
    AWSResponse UpdateVodSource =
      UpdateVodSourceResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVodSourceResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "SourceLocationName")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> ( x Core..?> "HttpPackageConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "VodSourceName")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVodSource where
  hashWithSalt salt' UpdateVodSource' {..} =
    salt'
      `Prelude.hashWithSalt` httpPackageConfigurations
      `Prelude.hashWithSalt` vodSourceName
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData UpdateVodSource where
  rnf UpdateVodSource' {..} =
    Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf vodSourceName

instance Core.ToHeaders UpdateVodSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateVodSource where
  toJSON UpdateVodSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "HttpPackageConfigurations"
                  Core..= httpPackageConfigurations
              )
          ]
      )

instance Core.ToPath UpdateVodSource where
  toPath UpdateVodSource' {..} =
    Prelude.mconcat
      [ "/sourceLocation/",
        Core.toBS sourceLocationName,
        "/vodSource/",
        Core.toBS vodSourceName
      ]

instance Core.ToQuery UpdateVodSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVodSourceResponse' smart constructor.
data UpdateVodSourceResponse = UpdateVodSourceResponse'
  { -- | The timestamp that indicates when the VOD source was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the source location associated with the VOD source.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the VOD source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the VOD source.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The HTTP package configurations.
    httpPackageConfigurations :: Prelude.Maybe [HttpPackageConfiguration],
    -- | The name of the VOD source.
    vodSourceName :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the VOD source.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'creationTime', 'updateVodSourceResponse_creationTime' - The timestamp that indicates when the VOD source was created.
--
-- 'sourceLocationName', 'updateVodSourceResponse_sourceLocationName' - The name of the source location associated with the VOD source.
--
-- 'arn', 'updateVodSourceResponse_arn' - The ARN of the VOD source.
--
-- 'lastModifiedTime', 'updateVodSourceResponse_lastModifiedTime' - The ARN for the VOD source.
--
-- 'httpPackageConfigurations', 'updateVodSourceResponse_httpPackageConfigurations' - The HTTP package configurations.
--
-- 'vodSourceName', 'updateVodSourceResponse_vodSourceName' - The name of the VOD source.
--
-- 'tags', 'updateVodSourceResponse_tags' - The tags assigned to the VOD source.
--
-- 'httpStatus', 'updateVodSourceResponse_httpStatus' - The response's http status code.
newUpdateVodSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVodSourceResponse
newUpdateVodSourceResponse pHttpStatus_ =
  UpdateVodSourceResponse'
    { creationTime =
        Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      httpPackageConfigurations = Prelude.Nothing,
      vodSourceName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp that indicates when the VOD source was created.
updateVodSourceResponse_creationTime :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.UTCTime)
updateVodSourceResponse_creationTime = Lens.lens (\UpdateVodSourceResponse' {creationTime} -> creationTime) (\s@UpdateVodSourceResponse' {} a -> s {creationTime = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the source location associated with the VOD source.
updateVodSourceResponse_sourceLocationName :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.Text)
updateVodSourceResponse_sourceLocationName = Lens.lens (\UpdateVodSourceResponse' {sourceLocationName} -> sourceLocationName) (\s@UpdateVodSourceResponse' {} a -> s {sourceLocationName = a} :: UpdateVodSourceResponse)

-- | The ARN of the VOD source.
updateVodSourceResponse_arn :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.Text)
updateVodSourceResponse_arn = Lens.lens (\UpdateVodSourceResponse' {arn} -> arn) (\s@UpdateVodSourceResponse' {} a -> s {arn = a} :: UpdateVodSourceResponse)

-- | The ARN for the VOD source.
updateVodSourceResponse_lastModifiedTime :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.UTCTime)
updateVodSourceResponse_lastModifiedTime = Lens.lens (\UpdateVodSourceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateVodSourceResponse' {} a -> s {lastModifiedTime = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Core._Time

-- | The HTTP package configurations.
updateVodSourceResponse_httpPackageConfigurations :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe [HttpPackageConfiguration])
updateVodSourceResponse_httpPackageConfigurations = Lens.lens (\UpdateVodSourceResponse' {httpPackageConfigurations} -> httpPackageConfigurations) (\s@UpdateVodSourceResponse' {} a -> s {httpPackageConfigurations = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the VOD source.
updateVodSourceResponse_vodSourceName :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe Prelude.Text)
updateVodSourceResponse_vodSourceName = Lens.lens (\UpdateVodSourceResponse' {vodSourceName} -> vodSourceName) (\s@UpdateVodSourceResponse' {} a -> s {vodSourceName = a} :: UpdateVodSourceResponse)

-- | The tags assigned to the VOD source.
updateVodSourceResponse_tags :: Lens.Lens' UpdateVodSourceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateVodSourceResponse_tags = Lens.lens (\UpdateVodSourceResponse' {tags} -> tags) (\s@UpdateVodSourceResponse' {} a -> s {tags = a} :: UpdateVodSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateVodSourceResponse_httpStatus :: Lens.Lens' UpdateVodSourceResponse Prelude.Int
updateVodSourceResponse_httpStatus = Lens.lens (\UpdateVodSourceResponse' {httpStatus} -> httpStatus) (\s@UpdateVodSourceResponse' {} a -> s {httpStatus = a} :: UpdateVodSourceResponse)

instance Prelude.NFData UpdateVodSourceResponse where
  rnf UpdateVodSourceResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vodSourceName
      `Prelude.seq` Prelude.rnf httpPackageConfigurations
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf sourceLocationName
