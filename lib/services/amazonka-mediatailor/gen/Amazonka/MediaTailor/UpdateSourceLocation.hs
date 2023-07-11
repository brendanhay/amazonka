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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a source location. A source location is a container for sources.
-- For more information about source locations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-source-locations.html Working with source locations>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.UpdateSourceLocation
  ( -- * Creating a Request
    UpdateSourceLocation (..),
    newUpdateSourceLocation,

    -- * Request Lenses
    updateSourceLocation_accessConfiguration,
    updateSourceLocation_defaultSegmentDeliveryConfiguration,
    updateSourceLocation_segmentDeliveryConfigurations,
    updateSourceLocation_httpConfiguration,
    updateSourceLocation_sourceLocationName,

    -- * Destructuring the Response
    UpdateSourceLocationResponse (..),
    newUpdateSourceLocationResponse,

    -- * Response Lenses
    updateSourceLocationResponse_accessConfiguration,
    updateSourceLocationResponse_arn,
    updateSourceLocationResponse_creationTime,
    updateSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    updateSourceLocationResponse_httpConfiguration,
    updateSourceLocationResponse_lastModifiedTime,
    updateSourceLocationResponse_segmentDeliveryConfigurations,
    updateSourceLocationResponse_sourceLocationName,
    updateSourceLocationResponse_tags,
    updateSourceLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSourceLocation' smart constructor.
data UpdateSourceLocation = UpdateSourceLocation'
  { -- | Access configuration parameters. Configures the type of authentication
    -- used to access content from your source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The optional configuration for the host server that serves segments.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | A list of the segment delivery configurations associated with this
    -- resource.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | The HTTP configuration for the source location.
    httpConfiguration :: HttpConfiguration,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text
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
-- 'accessConfiguration', 'updateSourceLocation_accessConfiguration' - Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'updateSourceLocation_defaultSegmentDeliveryConfiguration' - The optional configuration for the host server that serves segments.
--
-- 'segmentDeliveryConfigurations', 'updateSourceLocation_segmentDeliveryConfigurations' - A list of the segment delivery configurations associated with this
-- resource.
--
-- 'httpConfiguration', 'updateSourceLocation_httpConfiguration' - The HTTP configuration for the source location.
--
-- 'sourceLocationName', 'updateSourceLocation_sourceLocationName' - The name of the source location.
newUpdateSourceLocation ::
  -- | 'httpConfiguration'
  HttpConfiguration ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  UpdateSourceLocation
newUpdateSourceLocation
  pHttpConfiguration_
  pSourceLocationName_ =
    UpdateSourceLocation'
      { accessConfiguration =
          Prelude.Nothing,
        defaultSegmentDeliveryConfiguration =
          Prelude.Nothing,
        segmentDeliveryConfigurations = Prelude.Nothing,
        httpConfiguration = pHttpConfiguration_,
        sourceLocationName = pSourceLocationName_
      }

-- | Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
updateSourceLocation_accessConfiguration :: Lens.Lens' UpdateSourceLocation (Prelude.Maybe AccessConfiguration)
updateSourceLocation_accessConfiguration = Lens.lens (\UpdateSourceLocation' {accessConfiguration} -> accessConfiguration) (\s@UpdateSourceLocation' {} a -> s {accessConfiguration = a} :: UpdateSourceLocation)

-- | The optional configuration for the host server that serves segments.
updateSourceLocation_defaultSegmentDeliveryConfiguration :: Lens.Lens' UpdateSourceLocation (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
updateSourceLocation_defaultSegmentDeliveryConfiguration = Lens.lens (\UpdateSourceLocation' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@UpdateSourceLocation' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: UpdateSourceLocation)

-- | A list of the segment delivery configurations associated with this
-- resource.
updateSourceLocation_segmentDeliveryConfigurations :: Lens.Lens' UpdateSourceLocation (Prelude.Maybe [SegmentDeliveryConfiguration])
updateSourceLocation_segmentDeliveryConfigurations = Lens.lens (\UpdateSourceLocation' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@UpdateSourceLocation' {} a -> s {segmentDeliveryConfigurations = a} :: UpdateSourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP configuration for the source location.
updateSourceLocation_httpConfiguration :: Lens.Lens' UpdateSourceLocation HttpConfiguration
updateSourceLocation_httpConfiguration = Lens.lens (\UpdateSourceLocation' {httpConfiguration} -> httpConfiguration) (\s@UpdateSourceLocation' {} a -> s {httpConfiguration = a} :: UpdateSourceLocation)

-- | The name of the source location.
updateSourceLocation_sourceLocationName :: Lens.Lens' UpdateSourceLocation Prelude.Text
updateSourceLocation_sourceLocationName = Lens.lens (\UpdateSourceLocation' {sourceLocationName} -> sourceLocationName) (\s@UpdateSourceLocation' {} a -> s {sourceLocationName = a} :: UpdateSourceLocation)

instance Core.AWSRequest UpdateSourceLocation where
  type
    AWSResponse UpdateSourceLocation =
      UpdateSourceLocationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSourceLocationResponse'
            Prelude.<$> (x Data..?> "AccessConfiguration")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DefaultSegmentDeliveryConfiguration")
            Prelude.<*> (x Data..?> "HttpConfiguration")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> ( x
                            Data..?> "SegmentDeliveryConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSourceLocation where
  hashWithSalt _salt UpdateSourceLocation' {..} =
    _salt
      `Prelude.hashWithSalt` accessConfiguration
      `Prelude.hashWithSalt` defaultSegmentDeliveryConfiguration
      `Prelude.hashWithSalt` segmentDeliveryConfigurations
      `Prelude.hashWithSalt` httpConfiguration
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData UpdateSourceLocation where
  rnf UpdateSourceLocation' {..} =
    Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf httpConfiguration
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders UpdateSourceLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSourceLocation where
  toJSON UpdateSourceLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessConfiguration" Data..=)
              Prelude.<$> accessConfiguration,
            ("DefaultSegmentDeliveryConfiguration" Data..=)
              Prelude.<$> defaultSegmentDeliveryConfiguration,
            ("SegmentDeliveryConfigurations" Data..=)
              Prelude.<$> segmentDeliveryConfigurations,
            Prelude.Just
              ("HttpConfiguration" Data..= httpConfiguration)
          ]
      )

instance Data.ToPath UpdateSourceLocation where
  toPath UpdateSourceLocation' {..} =
    Prelude.mconcat
      ["/sourceLocation/", Data.toBS sourceLocationName]

instance Data.ToQuery UpdateSourceLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSourceLocationResponse' smart constructor.
data UpdateSourceLocationResponse = UpdateSourceLocationResponse'
  { -- | Access configuration parameters. Configures the type of authentication
    -- used to access content from your source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The Amazon Resource Name (ARN) associated with the source location.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp that indicates when the source location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The optional configuration for the host server that serves segments.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | The HTTP configuration for the source location.
    httpConfiguration :: Prelude.Maybe HttpConfiguration,
    -- | The timestamp that indicates when the source location was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The segment delivery configurations for the source location. For
    -- information about MediaTailor configurations, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | The name of the source location.
    sourceLocationName :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the source location. Tags are key-value pairs that
    -- you can associate with Amazon resources to help with organization,
    -- access control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'accessConfiguration', 'updateSourceLocationResponse_accessConfiguration' - Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
--
-- 'arn', 'updateSourceLocationResponse_arn' - The Amazon Resource Name (ARN) associated with the source location.
--
-- 'creationTime', 'updateSourceLocationResponse_creationTime' - The timestamp that indicates when the source location was created.
--
-- 'defaultSegmentDeliveryConfiguration', 'updateSourceLocationResponse_defaultSegmentDeliveryConfiguration' - The optional configuration for the host server that serves segments.
--
-- 'httpConfiguration', 'updateSourceLocationResponse_httpConfiguration' - The HTTP configuration for the source location.
--
-- 'lastModifiedTime', 'updateSourceLocationResponse_lastModifiedTime' - The timestamp that indicates when the source location was last modified.
--
-- 'segmentDeliveryConfigurations', 'updateSourceLocationResponse_segmentDeliveryConfigurations' - The segment delivery configurations for the source location. For
-- information about MediaTailor configurations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
--
-- 'sourceLocationName', 'updateSourceLocationResponse_sourceLocationName' - The name of the source location.
--
-- 'tags', 'updateSourceLocationResponse_tags' - The tags to assign to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpStatus', 'updateSourceLocationResponse_httpStatus' - The response's http status code.
newUpdateSourceLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSourceLocationResponse
newUpdateSourceLocationResponse pHttpStatus_ =
  UpdateSourceLocationResponse'
    { accessConfiguration =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      defaultSegmentDeliveryConfiguration =
        Prelude.Nothing,
      httpConfiguration = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      segmentDeliveryConfigurations =
        Prelude.Nothing,
      sourceLocationName = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
updateSourceLocationResponse_accessConfiguration :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe AccessConfiguration)
updateSourceLocationResponse_accessConfiguration = Lens.lens (\UpdateSourceLocationResponse' {accessConfiguration} -> accessConfiguration) (\s@UpdateSourceLocationResponse' {} a -> s {accessConfiguration = a} :: UpdateSourceLocationResponse)

-- | The Amazon Resource Name (ARN) associated with the source location.
updateSourceLocationResponse_arn :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.Text)
updateSourceLocationResponse_arn = Lens.lens (\UpdateSourceLocationResponse' {arn} -> arn) (\s@UpdateSourceLocationResponse' {} a -> s {arn = a} :: UpdateSourceLocationResponse)

-- | The timestamp that indicates when the source location was created.
updateSourceLocationResponse_creationTime :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
updateSourceLocationResponse_creationTime = Lens.lens (\UpdateSourceLocationResponse' {creationTime} -> creationTime) (\s@UpdateSourceLocationResponse' {} a -> s {creationTime = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Data._Time

-- | The optional configuration for the host server that serves segments.
updateSourceLocationResponse_defaultSegmentDeliveryConfiguration :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
updateSourceLocationResponse_defaultSegmentDeliveryConfiguration = Lens.lens (\UpdateSourceLocationResponse' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@UpdateSourceLocationResponse' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: UpdateSourceLocationResponse)

-- | The HTTP configuration for the source location.
updateSourceLocationResponse_httpConfiguration :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe HttpConfiguration)
updateSourceLocationResponse_httpConfiguration = Lens.lens (\UpdateSourceLocationResponse' {httpConfiguration} -> httpConfiguration) (\s@UpdateSourceLocationResponse' {} a -> s {httpConfiguration = a} :: UpdateSourceLocationResponse)

-- | The timestamp that indicates when the source location was last modified.
updateSourceLocationResponse_lastModifiedTime :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
updateSourceLocationResponse_lastModifiedTime = Lens.lens (\UpdateSourceLocationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdateSourceLocationResponse' {} a -> s {lastModifiedTime = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Data._Time

-- | The segment delivery configurations for the source location. For
-- information about MediaTailor configurations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
updateSourceLocationResponse_segmentDeliveryConfigurations :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe [SegmentDeliveryConfiguration])
updateSourceLocationResponse_segmentDeliveryConfigurations = Lens.lens (\UpdateSourceLocationResponse' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@UpdateSourceLocationResponse' {} a -> s {segmentDeliveryConfigurations = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the source location.
updateSourceLocationResponse_sourceLocationName :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe Prelude.Text)
updateSourceLocationResponse_sourceLocationName = Lens.lens (\UpdateSourceLocationResponse' {sourceLocationName} -> sourceLocationName) (\s@UpdateSourceLocationResponse' {} a -> s {sourceLocationName = a} :: UpdateSourceLocationResponse)

-- | The tags to assign to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
updateSourceLocationResponse_tags :: Lens.Lens' UpdateSourceLocationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateSourceLocationResponse_tags = Lens.lens (\UpdateSourceLocationResponse' {tags} -> tags) (\s@UpdateSourceLocationResponse' {} a -> s {tags = a} :: UpdateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateSourceLocationResponse_httpStatus :: Lens.Lens' UpdateSourceLocationResponse Prelude.Int
updateSourceLocationResponse_httpStatus = Lens.lens (\UpdateSourceLocationResponse' {httpStatus} -> httpStatus) (\s@UpdateSourceLocationResponse' {} a -> s {httpStatus = a} :: UpdateSourceLocationResponse)

instance Prelude.NFData UpdateSourceLocationResponse where
  rnf UpdateSourceLocationResponse' {..} =
    Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf httpConfiguration
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
