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
-- Module      : Amazonka.MediaTailor.UpdateSourceLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a source location on a specific channel.
module Amazonka.MediaTailor.UpdateSourceLocation
  ( -- * Creating a Request
    UpdateSourceLocation (..),
    newUpdateSourceLocation,

    -- * Request Lenses
    updateSourceLocation_segmentDeliveryConfigurations,
    updateSourceLocation_accessConfiguration,
    updateSourceLocation_defaultSegmentDeliveryConfiguration,
    updateSourceLocation_sourceLocationName,
    updateSourceLocation_httpConfiguration,

    -- * Destructuring the Response
    UpdateSourceLocationResponse (..),
    newUpdateSourceLocationResponse,

    -- * Response Lenses
    updateSourceLocationResponse_tags,
    updateSourceLocationResponse_segmentDeliveryConfigurations,
    updateSourceLocationResponse_arn,
    updateSourceLocationResponse_accessConfiguration,
    updateSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    updateSourceLocationResponse_lastModifiedTime,
    updateSourceLocationResponse_creationTime,
    updateSourceLocationResponse_sourceLocationName,
    updateSourceLocationResponse_httpConfiguration,
    updateSourceLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSourceLocation' smart constructor.
data UpdateSourceLocation = UpdateSourceLocation'
  { -- | A list of the segment delivery configurations associated with this
    -- resource.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | Access configuration parameters. Configures the type of authentication
    -- used to access content from your source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The optional configuration for the host server that serves segments.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | The identifier for the source location you are working on.
    sourceLocationName :: Prelude.Text,
    -- | The HTTP configuration for the source location.
    httpConfiguration :: HttpConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSourceLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentDeliveryConfigurations', 'updateSourceLocation_segmentDeliveryConfigurations' - A list of the segment delivery configurations associated with this
-- resource.
--
-- 'accessConfiguration', 'updateSourceLocation_accessConfiguration' - Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'updateSourceLocation_defaultSegmentDeliveryConfiguration' - The optional configuration for the host server that serves segments.
--
-- 'sourceLocationName', 'updateSourceLocation_sourceLocationName' - The identifier for the source location you are working on.
--
-- 'httpConfiguration', 'updateSourceLocation_httpConfiguration' - The HTTP configuration for the source location.
newUpdateSourceLocation ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'httpConfiguration'
  HttpConfiguration ->
  UpdateSourceLocation
newUpdateSourceLocation
  pSourceLocationName_
  pHttpConfiguration_ =
    UpdateSourceLocation'
      { segmentDeliveryConfigurations =
          Prelude.Nothing,
        accessConfiguration = Prelude.Nothing,
        defaultSegmentDeliveryConfiguration =
          Prelude.Nothing,
        sourceLocationName = pSourceLocationName_,
        httpConfiguration = pHttpConfiguration_
      }

-- | A list of the segment delivery configurations associated with this
-- resource.
updateSourceLocation_segmentDeliveryConfigurations :: Lens.Lens' UpdateSourceLocation (Prelude.Maybe [SegmentDeliveryConfiguration])
updateSourceLocation_segmentDeliveryConfigurations = Lens.lens (\UpdateSourceLocation' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@UpdateSourceLocation' {} a -> s {segmentDeliveryConfigurations = a} :: UpdateSourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
updateSourceLocation_accessConfiguration :: Lens.Lens' UpdateSourceLocation (Prelude.Maybe AccessConfiguration)
updateSourceLocation_accessConfiguration = Lens.lens (\UpdateSourceLocation' {accessConfiguration} -> accessConfiguration) (\s@UpdateSourceLocation' {} a -> s {accessConfiguration = a} :: UpdateSourceLocation)

-- | The optional configuration for the host server that serves segments.
updateSourceLocation_defaultSegmentDeliveryConfiguration :: Lens.Lens' UpdateSourceLocation (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
updateSourceLocation_defaultSegmentDeliveryConfiguration = Lens.lens (\UpdateSourceLocation' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@UpdateSourceLocation' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: UpdateSourceLocation)

-- | The identifier for the source location you are working on.
updateSourceLocation_sourceLocationName :: Lens.Lens' UpdateSourceLocation Prelude.Text
updateSourceLocation_sourceLocationName = Lens.lens (\UpdateSourceLocation' {sourceLocationName} -> sourceLocationName) (\s@UpdateSourceLocation' {} a -> s {sourceLocationName = a} :: UpdateSourceLocation)

-- | The HTTP configuration for the source location.
updateSourceLocation_httpConfiguration :: Lens.Lens' UpdateSourceLocation HttpConfiguration
updateSourceLocation_httpConfiguration = Lens.lens (\UpdateSourceLocation' {httpConfiguration} -> httpConfiguration) (\s@UpdateSourceLocation' {} a -> s {httpConfiguration = a} :: UpdateSourceLocation)

instance Core.AWSRequest UpdateSourceLocation where
  type
    AWSResponse UpdateSourceLocation =
      UpdateSourceLocationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSourceLocationResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "SegmentDeliveryConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "AccessConfiguration")
            Prelude.<*> (x Core..?> "DefaultSegmentDeliveryConfiguration")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "SourceLocationName")
            Prelude.<*> (x Core..?> "HttpConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSourceLocation where
  hashWithSalt _salt UpdateSourceLocation' {..} =
    _salt
      `Prelude.hashWithSalt` segmentDeliveryConfigurations
      `Prelude.hashWithSalt` accessConfiguration
      `Prelude.hashWithSalt` defaultSegmentDeliveryConfiguration
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` httpConfiguration

instance Prelude.NFData UpdateSourceLocation where
  rnf UpdateSourceLocation' {..} =
    Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpConfiguration

instance Core.ToHeaders UpdateSourceLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSourceLocation where
  toJSON UpdateSourceLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SegmentDeliveryConfigurations" Core..=)
              Prelude.<$> segmentDeliveryConfigurations,
            ("AccessConfiguration" Core..=)
              Prelude.<$> accessConfiguration,
            ("DefaultSegmentDeliveryConfiguration" Core..=)
              Prelude.<$> defaultSegmentDeliveryConfiguration,
            Prelude.Just
              ("HttpConfiguration" Core..= httpConfiguration)
          ]
      )

instance Core.ToPath UpdateSourceLocation where
  toPath UpdateSourceLocation' {..} =
    Prelude.mconcat
      ["/sourceLocation/", Core.toBS sourceLocationName]

instance Core.ToQuery UpdateSourceLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSourceLocationResponse' smart constructor.
data UpdateSourceLocationResponse = UpdateSourceLocationResponse'
  { -- | The tags assigned to the source location.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of the segment delivery configurations associated with this
    -- resource.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | The ARN of the source location.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The access configuration for the source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The default segment delivery configuration settings.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | The timestamp that indicates when the source location was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The timestamp that indicates when the source location was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The HTTP package configuration settings for the source location.
    httpConfiguration :: Prelude.Maybe HttpConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSourceLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateSourceLocationResponse_tags' - The tags assigned to the source location.
--
-- 'segmentDeliveryConfigurations', 'updateSourceLocationResponse_segmentDeliveryConfigurations' - A list of the segment delivery configurations associated with this
-- resource.
--
-- 'arn', 'updateSourceLocationResponse_arn' - The ARN of the source location.
--
-- 'accessConfiguration', 'updateSourceLocationResponse_accessConfiguration' - The access configuration for the source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'updateSourceLocationResponse_defaultSegmentDeliveryConfiguration' - The default segment delivery configuration settings.
--
-- 'lastModifiedTime', 'updateSourceLocationResponse_lastModifiedTime' - The timestamp that indicates when the source location was last modified.
--
-- 'creationTime', 'updateSourceLocationResponse_creationTime' - The timestamp that indicates when the source location was created.
--
-- 'sourceLocationName', 'updateSourceLocationResponse_sourceLocationName' - The name of the source location.
--
-- 'httpConfiguration', 'updateSourceLocationResponse_httpConfiguration' - The HTTP package configuration settings for the source location.
--
-- 'httpStatus', 'updateSourceLocationResponse_httpStatus' - The response's http status code.
newUpdateSourceLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSourceLocationResponse
newUpdateSourceLocationResponse pHttpStatus_ =
  UpdateSourceLocationResponse'
    { tags =
        Prelude.Nothing,
      segmentDeliveryConfigurations =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      accessConfiguration = Prelude.Nothing,
      defaultSegmentDeliveryConfiguration =
        Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      httpConfiguration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The tags assigned to the source location.
updateSourceLocationResponse_tags :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateSourceLocationResponse_tags = Lens.lens (\UpdateSourceLocationResponse' {tags} -> tags) (\s@UpdateSourceLocationResponse' {} a -> s {tags = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the segment delivery configurations associated with this
-- resource.
updateSourceLocationResponse_segmentDeliveryConfigurations :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe [SegmentDeliveryConfiguration])
updateSourceLocationResponse_segmentDeliveryConfigurations = Lens.lens (\UpdateSourceLocationResponse' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@UpdateSourceLocationResponse' {} a -> s {segmentDeliveryConfigurations = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the source location.
updateSourceLocationResponse_arn :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.Text)
updateSourceLocationResponse_arn = Lens.lens (\UpdateSourceLocationResponse' {arn} -> arn) (\s@UpdateSourceLocationResponse' {} a -> s {arn = a} :: UpdateSourceLocationResponse)

-- | The access configuration for the source location.
updateSourceLocationResponse_accessConfiguration :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe AccessConfiguration)
updateSourceLocationResponse_accessConfiguration = Lens.lens (\UpdateSourceLocationResponse' {accessConfiguration} -> accessConfiguration) (\s@UpdateSourceLocationResponse' {} a -> s {accessConfiguration = a} :: UpdateSourceLocationResponse)

-- | The default segment delivery configuration settings.
updateSourceLocationResponse_defaultSegmentDeliveryConfiguration :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
updateSourceLocationResponse_defaultSegmentDeliveryConfiguration = Lens.lens (\UpdateSourceLocationResponse' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@UpdateSourceLocationResponse' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: UpdateSourceLocationResponse)

-- | The timestamp that indicates when the source location was last modified.
updateSourceLocationResponse_lastModifiedTime :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
updateSourceLocationResponse_lastModifiedTime = Lens.lens (\UpdateSourceLocationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateSourceLocationResponse' {} a -> s {lastModifiedTime = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp that indicates when the source location was created.
updateSourceLocationResponse_creationTime :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
updateSourceLocationResponse_creationTime = Lens.lens (\UpdateSourceLocationResponse' {creationTime} -> creationTime) (\s@UpdateSourceLocationResponse' {} a -> s {creationTime = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the source location.
updateSourceLocationResponse_sourceLocationName :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.Text)
updateSourceLocationResponse_sourceLocationName = Lens.lens (\UpdateSourceLocationResponse' {sourceLocationName} -> sourceLocationName) (\s@UpdateSourceLocationResponse' {} a -> s {sourceLocationName = a} :: UpdateSourceLocationResponse)

-- | The HTTP package configuration settings for the source location.
updateSourceLocationResponse_httpConfiguration :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe HttpConfiguration)
updateSourceLocationResponse_httpConfiguration = Lens.lens (\UpdateSourceLocationResponse' {httpConfiguration} -> httpConfiguration) (\s@UpdateSourceLocationResponse' {} a -> s {httpConfiguration = a} :: UpdateSourceLocationResponse)

-- | The response's http status code.
updateSourceLocationResponse_httpStatus :: Lens.Lens' UpdateSourceLocationResponse Prelude.Int
updateSourceLocationResponse_httpStatus = Lens.lens (\UpdateSourceLocationResponse' {httpStatus} -> httpStatus) (\s@UpdateSourceLocationResponse' {} a -> s {httpStatus = a} :: UpdateSourceLocationResponse)

instance Prelude.NFData UpdateSourceLocationResponse where
  rnf UpdateSourceLocationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
