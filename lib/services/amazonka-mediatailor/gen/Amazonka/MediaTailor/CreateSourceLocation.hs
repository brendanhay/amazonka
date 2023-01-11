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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a source location. A source location is a container for sources.
-- For more information about source locations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-source-locations.html Working with source locations>
-- in the /MediaTailor User Guide/.
module Amazonka.MediaTailor.CreateSourceLocation
  ( -- * Creating a Request
    CreateSourceLocation (..),
    newCreateSourceLocation,

    -- * Request Lenses
    createSourceLocation_accessConfiguration,
    createSourceLocation_defaultSegmentDeliveryConfiguration,
    createSourceLocation_segmentDeliveryConfigurations,
    createSourceLocation_tags,
    createSourceLocation_httpConfiguration,
    createSourceLocation_sourceLocationName,

    -- * Destructuring the Response
    CreateSourceLocationResponse (..),
    newCreateSourceLocationResponse,

    -- * Response Lenses
    createSourceLocationResponse_accessConfiguration,
    createSourceLocationResponse_arn,
    createSourceLocationResponse_creationTime,
    createSourceLocationResponse_defaultSegmentDeliveryConfiguration,
    createSourceLocationResponse_httpConfiguration,
    createSourceLocationResponse_lastModifiedTime,
    createSourceLocationResponse_segmentDeliveryConfigurations,
    createSourceLocationResponse_sourceLocationName,
    createSourceLocationResponse_tags,
    createSourceLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSourceLocation' smart constructor.
data CreateSourceLocation = CreateSourceLocation'
  { -- | Access configuration parameters. Configures the type of authentication
    -- used to access content from your source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The optional configuration for the server that serves segments.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | A list of the segment delivery configurations associated with this
    -- resource.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | The tags to assign to the source location. Tags are key-value pairs that
    -- you can associate with Amazon resources to help with organization,
    -- access control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source\'s HTTP package configurations.
    httpConfiguration :: HttpConfiguration,
    -- | The name associated with the source location.
    sourceLocationName :: Prelude.Text
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
-- 'accessConfiguration', 'createSourceLocation_accessConfiguration' - Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'createSourceLocation_defaultSegmentDeliveryConfiguration' - The optional configuration for the server that serves segments.
--
-- 'segmentDeliveryConfigurations', 'createSourceLocation_segmentDeliveryConfigurations' - A list of the segment delivery configurations associated with this
-- resource.
--
-- 'tags', 'createSourceLocation_tags' - The tags to assign to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpConfiguration', 'createSourceLocation_httpConfiguration' - The source\'s HTTP package configurations.
--
-- 'sourceLocationName', 'createSourceLocation_sourceLocationName' - The name associated with the source location.
newCreateSourceLocation ::
  -- | 'httpConfiguration'
  HttpConfiguration ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  CreateSourceLocation
newCreateSourceLocation
  pHttpConfiguration_
  pSourceLocationName_ =
    CreateSourceLocation'
      { accessConfiguration =
          Prelude.Nothing,
        defaultSegmentDeliveryConfiguration =
          Prelude.Nothing,
        segmentDeliveryConfigurations = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpConfiguration = pHttpConfiguration_,
        sourceLocationName = pSourceLocationName_
      }

-- | Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
createSourceLocation_accessConfiguration :: Lens.Lens' CreateSourceLocation (Prelude.Maybe AccessConfiguration)
createSourceLocation_accessConfiguration = Lens.lens (\CreateSourceLocation' {accessConfiguration} -> accessConfiguration) (\s@CreateSourceLocation' {} a -> s {accessConfiguration = a} :: CreateSourceLocation)

-- | The optional configuration for the server that serves segments.
createSourceLocation_defaultSegmentDeliveryConfiguration :: Lens.Lens' CreateSourceLocation (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
createSourceLocation_defaultSegmentDeliveryConfiguration = Lens.lens (\CreateSourceLocation' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@CreateSourceLocation' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: CreateSourceLocation)

-- | A list of the segment delivery configurations associated with this
-- resource.
createSourceLocation_segmentDeliveryConfigurations :: Lens.Lens' CreateSourceLocation (Prelude.Maybe [SegmentDeliveryConfiguration])
createSourceLocation_segmentDeliveryConfigurations = Lens.lens (\CreateSourceLocation' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@CreateSourceLocation' {} a -> s {segmentDeliveryConfigurations = a} :: CreateSourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | The tags to assign to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
createSourceLocation_tags :: Lens.Lens' CreateSourceLocation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSourceLocation_tags = Lens.lens (\CreateSourceLocation' {tags} -> tags) (\s@CreateSourceLocation' {} a -> s {tags = a} :: CreateSourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | The source\'s HTTP package configurations.
createSourceLocation_httpConfiguration :: Lens.Lens' CreateSourceLocation HttpConfiguration
createSourceLocation_httpConfiguration = Lens.lens (\CreateSourceLocation' {httpConfiguration} -> httpConfiguration) (\s@CreateSourceLocation' {} a -> s {httpConfiguration = a} :: CreateSourceLocation)

-- | The name associated with the source location.
createSourceLocation_sourceLocationName :: Lens.Lens' CreateSourceLocation Prelude.Text
createSourceLocation_sourceLocationName = Lens.lens (\CreateSourceLocation' {sourceLocationName} -> sourceLocationName) (\s@CreateSourceLocation' {} a -> s {sourceLocationName = a} :: CreateSourceLocation)

instance Core.AWSRequest CreateSourceLocation where
  type
    AWSResponse CreateSourceLocation =
      CreateSourceLocationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSourceLocationResponse'
            Prelude.<$> (x Data..?> "AccessConfiguration")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DefaultSegmentDeliveryConfiguration")
            Prelude.<*> (x Data..?> "HttpConfiguration")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> ( x Data..?> "SegmentDeliveryConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "SourceLocationName")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSourceLocation where
  hashWithSalt _salt CreateSourceLocation' {..} =
    _salt `Prelude.hashWithSalt` accessConfiguration
      `Prelude.hashWithSalt` defaultSegmentDeliveryConfiguration
      `Prelude.hashWithSalt` segmentDeliveryConfigurations
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` httpConfiguration
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData CreateSourceLocation where
  rnf CreateSourceLocation' {..} =
    Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpConfiguration
      `Prelude.seq` Prelude.rnf sourceLocationName

instance Data.ToHeaders CreateSourceLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSourceLocation where
  toJSON CreateSourceLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessConfiguration" Data..=)
              Prelude.<$> accessConfiguration,
            ("DefaultSegmentDeliveryConfiguration" Data..=)
              Prelude.<$> defaultSegmentDeliveryConfiguration,
            ("SegmentDeliveryConfigurations" Data..=)
              Prelude.<$> segmentDeliveryConfigurations,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("HttpConfiguration" Data..= httpConfiguration)
          ]
      )

instance Data.ToPath CreateSourceLocation where
  toPath CreateSourceLocation' {..} =
    Prelude.mconcat
      ["/sourceLocation/", Data.toBS sourceLocationName]

instance Data.ToQuery CreateSourceLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSourceLocationResponse' smart constructor.
data CreateSourceLocationResponse = CreateSourceLocationResponse'
  { -- | Access configuration parameters. Configures the type of authentication
    -- used to access content from your source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The ARN to assign to the source location.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the source location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The optional configuration for the server that serves segments.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | The source\'s HTTP package configurations.
    httpConfiguration :: Prelude.Maybe HttpConfiguration,
    -- | The time the source location was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The segment delivery configurations for the source location. For
    -- information about MediaTailor configurations, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | The name to assign to the source location.
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
-- Create a value of 'CreateSourceLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessConfiguration', 'createSourceLocationResponse_accessConfiguration' - Access configuration parameters. Configures the type of authentication
-- used to access content from your source location.
--
-- 'arn', 'createSourceLocationResponse_arn' - The ARN to assign to the source location.
--
-- 'creationTime', 'createSourceLocationResponse_creationTime' - The time the source location was created.
--
-- 'defaultSegmentDeliveryConfiguration', 'createSourceLocationResponse_defaultSegmentDeliveryConfiguration' - The optional configuration for the server that serves segments.
--
-- 'httpConfiguration', 'createSourceLocationResponse_httpConfiguration' - The source\'s HTTP package configurations.
--
-- 'lastModifiedTime', 'createSourceLocationResponse_lastModifiedTime' - The time the source location was last modified.
--
-- 'segmentDeliveryConfigurations', 'createSourceLocationResponse_segmentDeliveryConfigurations' - The segment delivery configurations for the source location. For
-- information about MediaTailor configurations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
--
-- 'sourceLocationName', 'createSourceLocationResponse_sourceLocationName' - The name to assign to the source location.
--
-- 'tags', 'createSourceLocationResponse_tags' - The tags to assign to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'httpStatus', 'createSourceLocationResponse_httpStatus' - The response's http status code.
newCreateSourceLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSourceLocationResponse
newCreateSourceLocationResponse pHttpStatus_ =
  CreateSourceLocationResponse'
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
createSourceLocationResponse_accessConfiguration :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe AccessConfiguration)
createSourceLocationResponse_accessConfiguration = Lens.lens (\CreateSourceLocationResponse' {accessConfiguration} -> accessConfiguration) (\s@CreateSourceLocationResponse' {} a -> s {accessConfiguration = a} :: CreateSourceLocationResponse)

-- | The ARN to assign to the source location.
createSourceLocationResponse_arn :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.Text)
createSourceLocationResponse_arn = Lens.lens (\CreateSourceLocationResponse' {arn} -> arn) (\s@CreateSourceLocationResponse' {} a -> s {arn = a} :: CreateSourceLocationResponse)

-- | The time the source location was created.
createSourceLocationResponse_creationTime :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
createSourceLocationResponse_creationTime = Lens.lens (\CreateSourceLocationResponse' {creationTime} -> creationTime) (\s@CreateSourceLocationResponse' {} a -> s {creationTime = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Data._Time

-- | The optional configuration for the server that serves segments.
createSourceLocationResponse_defaultSegmentDeliveryConfiguration :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
createSourceLocationResponse_defaultSegmentDeliveryConfiguration = Lens.lens (\CreateSourceLocationResponse' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@CreateSourceLocationResponse' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: CreateSourceLocationResponse)

-- | The source\'s HTTP package configurations.
createSourceLocationResponse_httpConfiguration :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe HttpConfiguration)
createSourceLocationResponse_httpConfiguration = Lens.lens (\CreateSourceLocationResponse' {httpConfiguration} -> httpConfiguration) (\s@CreateSourceLocationResponse' {} a -> s {httpConfiguration = a} :: CreateSourceLocationResponse)

-- | The time the source location was last modified.
createSourceLocationResponse_lastModifiedTime :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.UTCTime)
createSourceLocationResponse_lastModifiedTime = Lens.lens (\CreateSourceLocationResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreateSourceLocationResponse' {} a -> s {lastModifiedTime = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Data._Time

-- | The segment delivery configurations for the source location. For
-- information about MediaTailor configurations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/configurations.html Working with configurations in AWS Elemental MediaTailor>.
createSourceLocationResponse_segmentDeliveryConfigurations :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe [SegmentDeliveryConfiguration])
createSourceLocationResponse_segmentDeliveryConfigurations = Lens.lens (\CreateSourceLocationResponse' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@CreateSourceLocationResponse' {} a -> s {segmentDeliveryConfigurations = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name to assign to the source location.
createSourceLocationResponse_sourceLocationName :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe Prelude.Text)
createSourceLocationResponse_sourceLocationName = Lens.lens (\CreateSourceLocationResponse' {sourceLocationName} -> sourceLocationName) (\s@CreateSourceLocationResponse' {} a -> s {sourceLocationName = a} :: CreateSourceLocationResponse)

-- | The tags to assign to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
createSourceLocationResponse_tags :: Lens.Lens' CreateSourceLocationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSourceLocationResponse_tags = Lens.lens (\CreateSourceLocationResponse' {tags} -> tags) (\s@CreateSourceLocationResponse' {} a -> s {tags = a} :: CreateSourceLocationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSourceLocationResponse_httpStatus :: Lens.Lens' CreateSourceLocationResponse Prelude.Int
createSourceLocationResponse_httpStatus = Lens.lens (\CreateSourceLocationResponse' {httpStatus} -> httpStatus) (\s@CreateSourceLocationResponse' {} a -> s {httpStatus = a} :: CreateSourceLocationResponse)

instance Prelude.NFData CreateSourceLocationResponse where
  rnf CreateSourceLocationResponse' {..} =
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
