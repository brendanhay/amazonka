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
-- Module      : Amazonka.MediaTailor.CreateSourceLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a source location on a specific channel.
module Amazonka.MediaTailor.CreateSourceLocation
  ( -- * Creating a Request
    CreateSourceLocation (..),
    newCreateSourceLocation,

    -- * Request Lenses
    createSourceLocation_tags,
    createSourceLocation_segmentDeliveryConfigurations,
    createSourceLocation_accessConfiguration,
    createSourceLocation_defaultSegmentDeliveryConfiguration,
    createSourceLocation_sourceLocationName,
    createSourceLocation_httpConfiguration,

    -- * Destructuring the Response
    CreateSourceLocationResponse (..),
    newCreateSourceLocationResponse,

    -- * Response Lenses
    createSourceLocationResponse_tags,
    createSourceLocationResponse_segmentDeliveryConfigurations,
    createSourceLocationResponse_arn,
    createSourceLocationResponse_accessConfiguration,
    createSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    createSourceLocationResponse_lastModifiedTime,
    createSourceLocationResponse_creationTime,
    createSourceLocationResponse_sourceLocationName,
    createSourceLocationResponse_httpConfiguration,
    createSourceLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSourceLocation' smart constructor.
data CreateSourceLocation = CreateSourceLocation'
  { -- | The tags to assign to the source location.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A list of the segment delivery configurations associated with this
    -- resource.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | Access configuration parameters. Configures the type of authentication
    -- used to access content from your source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The optional configuration for the server that serves segments.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | The identifier for the source location you are working on.
    sourceLocationName :: Prelude.Text,
    -- | The source\'s HTTP package configurations.
    httpConfiguration :: HttpConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSourceLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSourceLocation_tags' - The tags to assign to the source location.
--
-- 'segmentDeliveryConfigurations', 'createSourceLocation_segmentDeliveryConfigurations' - A list of the segment delivery configurations associated with this
-- resource.
--
-- 'accessConfiguration', 'createSourceLocation_accessConfiguration' - Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'createSourceLocation_defaultSegmentDeliveryConfiguration' - The optional configuration for the server that serves segments.
--
-- 'sourceLocationName', 'createSourceLocation_sourceLocationName' - The identifier for the source location you are working on.
--
-- 'httpConfiguration', 'createSourceLocation_httpConfiguration' - The source\'s HTTP package configurations.
newCreateSourceLocation ::
  -- | 'sourceLocationName'
  Prelude.Text ->
  -- | 'httpConfiguration'
  HttpConfiguration ->
  CreateSourceLocation
newCreateSourceLocation
  pSourceLocationName_
  pHttpConfiguration_ =
    CreateSourceLocation'
      { tags = Prelude.Nothing,
        segmentDeliveryConfigurations = Prelude.Nothing,
        accessConfiguration = Prelude.Nothing,
        defaultSegmentDeliveryConfiguration =
          Prelude.Nothing,
        sourceLocationName = pSourceLocationName_,
        httpConfiguration = pHttpConfiguration_
      }

-- | The tags to assign to the source location.
createSourceLocation_tags :: Lens.Lens' CreateSourceLocation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSourceLocation_tags = Lens.lens (\CreateSourceLocation' {tags} -> tags) (\s@CreateSourceLocation' {} a -> s {tags = a} :: CreateSourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | A list of the segment delivery configurations associated with this
-- resource.
createSourceLocation_segmentDeliveryConfigurations :: Lens.Lens' CreateSourceLocation (Prelude.Maybe [SegmentDeliveryConfiguration])
createSourceLocation_segmentDeliveryConfigurations = Lens.lens (\CreateSourceLocation' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@CreateSourceLocation' {} a -> s {segmentDeliveryConfigurations = a} :: CreateSourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
createSourceLocation_accessConfiguration :: Lens.Lens' CreateSourceLocation (Prelude.Maybe AccessConfiguration)
createSourceLocation_accessConfiguration = Lens.lens (\CreateSourceLocation' {accessConfiguration} -> accessConfiguration) (\s@CreateSourceLocation' {} a -> s {accessConfiguration = a} :: CreateSourceLocation)

-- | The optional configuration for the server that serves segments.
createSourceLocation_defaultSegmentDeliveryConfiguration :: Lens.Lens' CreateSourceLocation (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
createSourceLocation_defaultSegmentDeliveryConfiguration = Lens.lens (\CreateSourceLocation' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@CreateSourceLocation' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: CreateSourceLocation)

-- | The identifier for the source location you are working on.
createSourceLocation_sourceLocationName :: Lens.Lens' CreateSourceLocation Prelude.Text
createSourceLocation_sourceLocationName = Lens.lens (\CreateSourceLocation' {sourceLocationName} -> sourceLocationName) (\s@CreateSourceLocation' {} a -> s {sourceLocationName = a} :: CreateSourceLocation)

-- | The source\'s HTTP package configurations.
createSourceLocation_httpConfiguration :: Lens.Lens' CreateSourceLocation HttpConfiguration
createSourceLocation_httpConfiguration = Lens.lens (\CreateSourceLocation' {httpConfiguration} -> httpConfiguration) (\s@CreateSourceLocation' {} a -> s {httpConfiguration = a} :: CreateSourceLocation)

instance Core.AWSRequest CreateSourceLocation where
  type
    AWSResponse CreateSourceLocation =
      CreateSourceLocationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSourceLocationResponse'
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

instance Prelude.Hashable CreateSourceLocation where
  hashWithSalt _salt CreateSourceLocation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` segmentDeliveryConfigurations
      `Prelude.hashWithSalt` accessConfiguration
      `Prelude.hashWithSalt` defaultSegmentDeliveryConfiguration
      `Prelude.hashWithSalt` sourceLocationName
      `Prelude.hashWithSalt` httpConfiguration

instance Prelude.NFData CreateSourceLocation where
  rnf CreateSourceLocation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf sourceLocationName
      `Prelude.seq` Prelude.rnf httpConfiguration

instance Core.ToHeaders CreateSourceLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateSourceLocation where
  toJSON CreateSourceLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("SegmentDeliveryConfigurations" Core..=)
              Prelude.<$> segmentDeliveryConfigurations,
            ("AccessConfiguration" Core..=)
              Prelude.<$> accessConfiguration,
            ("DefaultSegmentDeliveryConfiguration" Core..=)
              Prelude.<$> defaultSegmentDeliveryConfiguration,
            Prelude.Just
              ("HttpConfiguration" Core..= httpConfiguration)
          ]
      )

instance Core.ToPath CreateSourceLocation where
  toPath CreateSourceLocation' {..} =
    Prelude.mconcat
      ["/sourceLocation/", Core.toBS sourceLocationName]

instance Core.ToQuery CreateSourceLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSourceLocationResponse' smart constructor.
data CreateSourceLocationResponse = CreateSourceLocationResponse'
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
-- Create a value of 'CreateSourceLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSourceLocationResponse_tags' - The tags assigned to the source location.
--
-- 'segmentDeliveryConfigurations', 'createSourceLocationResponse_segmentDeliveryConfigurations' - A list of the segment delivery configurations associated with this
-- resource.
--
-- 'arn', 'createSourceLocationResponse_arn' - The ARN of the source location.
--
-- 'accessConfiguration', 'createSourceLocationResponse_accessConfiguration' - The access configuration for the source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'createSourceLocationResponse_defaultSegmentDeliveryConfiguration' - The default segment delivery configuration settings.
--
-- 'lastModifiedTime', 'createSourceLocationResponse_lastModifiedTime' - The timestamp that indicates when the source location was last modified.
--
-- 'creationTime', 'createSourceLocationResponse_creationTime' - The timestamp that indicates when the source location was created.
--
-- 'sourceLocationName', 'createSourceLocationResponse_sourceLocationName' - The name of the source location.
--
-- 'httpConfiguration', 'createSourceLocationResponse_httpConfiguration' - The HTTP package configuration settings for the source location.
--
-- 'httpStatus', 'createSourceLocationResponse_httpStatus' - The response's http status code.
newCreateSourceLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSourceLocationResponse
newCreateSourceLocationResponse pHttpStatus_ =
  CreateSourceLocationResponse'
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
createSourceLocationResponse_tags :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSourceLocationResponse_tags = Lens.lens (\CreateSourceLocationResponse' {tags} -> tags) (\s@CreateSourceLocationResponse' {} a -> s {tags = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the segment delivery configurations associated with this
-- resource.
createSourceLocationResponse_segmentDeliveryConfigurations :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe [SegmentDeliveryConfiguration])
createSourceLocationResponse_segmentDeliveryConfigurations = Lens.lens (\CreateSourceLocationResponse' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@CreateSourceLocationResponse' {} a -> s {segmentDeliveryConfigurations = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the source location.
createSourceLocationResponse_arn :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.Text)
createSourceLocationResponse_arn = Lens.lens (\CreateSourceLocationResponse' {arn} -> arn) (\s@CreateSourceLocationResponse' {} a -> s {arn = a} :: CreateSourceLocationResponse)

-- | The access configuration for the source location.
createSourceLocationResponse_accessConfiguration :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe AccessConfiguration)
createSourceLocationResponse_accessConfiguration = Lens.lens (\CreateSourceLocationResponse' {accessConfiguration} -> accessConfiguration) (\s@CreateSourceLocationResponse' {} a -> s {accessConfiguration = a} :: CreateSourceLocationResponse)

-- | The default segment delivery configuration settings.
createSourceLocationResponse_defaultSegmentDeliveryConfiguration :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
createSourceLocationResponse_defaultSegmentDeliveryConfiguration = Lens.lens (\CreateSourceLocationResponse' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@CreateSourceLocationResponse' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: CreateSourceLocationResponse)

-- | The timestamp that indicates when the source location was last modified.
createSourceLocationResponse_lastModifiedTime :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
createSourceLocationResponse_lastModifiedTime = Lens.lens (\CreateSourceLocationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateSourceLocationResponse' {} a -> s {lastModifiedTime = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Core._Time

-- | The timestamp that indicates when the source location was created.
createSourceLocationResponse_creationTime :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
createSourceLocationResponse_creationTime = Lens.lens (\CreateSourceLocationResponse' {creationTime} -> creationTime) (\s@CreateSourceLocationResponse' {} a -> s {creationTime = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the source location.
createSourceLocationResponse_sourceLocationName :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.Text)
createSourceLocationResponse_sourceLocationName = Lens.lens (\CreateSourceLocationResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateSourceLocationResponse' {} a -> s {sourceLocationName = a} :: CreateSourceLocationResponse)

-- | The HTTP package configuration settings for the source location.
createSourceLocationResponse_httpConfiguration :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe HttpConfiguration)
createSourceLocationResponse_httpConfiguration = Lens.lens (\CreateSourceLocationResponse' {httpConfiguration} -> httpConfiguration) (\s@CreateSourceLocationResponse' {} a -> s {httpConfiguration = a} :: CreateSourceLocationResponse)

-- | The response's http status code.
createSourceLocationResponse_httpStatus :: Lens.Lens' CreateSourceLocationResponse Prelude.Int
createSourceLocationResponse_httpStatus = Lens.lens (\CreateSourceLocationResponse' {httpStatus} -> httpStatus) (\s@CreateSourceLocationResponse' {} a -> s {httpStatus = a} :: CreateSourceLocationResponse)

instance Prelude.NFData CreateSourceLocationResponse where
  rnf CreateSourceLocationResponse' {..} =
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
