{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaTailor.Types.SourceLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.SourceLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaTailor.Types.AccessConfiguration
import Amazonka.MediaTailor.Types.DefaultSegmentDeliveryConfiguration
import Amazonka.MediaTailor.Types.HttpConfiguration
import Amazonka.MediaTailor.Types.SegmentDeliveryConfiguration
import qualified Amazonka.Prelude as Prelude

-- | A source location is a container for sources. For more information about
-- source locations, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/channel-assembly-source-locations.html Working with source locations>
-- in the /MediaTailor User Guide/.
--
-- /See:/ 'newSourceLocation' smart constructor.
data SourceLocation = SourceLocation'
  { -- | The tags assigned to the source location. Tags are key-value pairs that
    -- you can associate with Amazon resources to help with organization,
    -- access control, and cost tracking. For more information, see
    -- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The segment delivery configurations for the source location.
    segmentDeliveryConfigurations :: Prelude.Maybe [SegmentDeliveryConfiguration],
    -- | The access configuration for the source location.
    accessConfiguration :: Prelude.Maybe AccessConfiguration,
    -- | The default segment delivery configuration.
    defaultSegmentDeliveryConfiguration :: Prelude.Maybe DefaultSegmentDeliveryConfiguration,
    -- | The timestamp that indicates when the source location was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The timestamp that indicates when the source location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the SourceLocation.
    arn :: Prelude.Text,
    -- | The HTTP configuration for the source location.
    httpConfiguration :: HttpConfiguration,
    -- | The name of the source location.
    sourceLocationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'sourceLocation_tags' - The tags assigned to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
--
-- 'segmentDeliveryConfigurations', 'sourceLocation_segmentDeliveryConfigurations' - The segment delivery configurations for the source location.
--
-- 'accessConfiguration', 'sourceLocation_accessConfiguration' - The access configuration for the source location.
--
-- 'defaultSegmentDeliveryConfiguration', 'sourceLocation_defaultSegmentDeliveryConfiguration' - The default segment delivery configuration.
--
-- 'lastModifiedTime', 'sourceLocation_lastModifiedTime' - The timestamp that indicates when the source location was last modified.
--
-- 'creationTime', 'sourceLocation_creationTime' - The timestamp that indicates when the source location was created.
--
-- 'arn', 'sourceLocation_arn' - The ARN of the SourceLocation.
--
-- 'httpConfiguration', 'sourceLocation_httpConfiguration' - The HTTP configuration for the source location.
--
-- 'sourceLocationName', 'sourceLocation_sourceLocationName' - The name of the source location.
newSourceLocation ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'httpConfiguration'
  HttpConfiguration ->
  -- | 'sourceLocationName'
  Prelude.Text ->
  SourceLocation
newSourceLocation
  pArn_
  pHttpConfiguration_
  pSourceLocationName_ =
    SourceLocation'
      { tags = Prelude.Nothing,
        segmentDeliveryConfigurations = Prelude.Nothing,
        accessConfiguration = Prelude.Nothing,
        defaultSegmentDeliveryConfiguration =
          Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        arn = pArn_,
        httpConfiguration = pHttpConfiguration_,
        sourceLocationName = pSourceLocationName_
      }

-- | The tags assigned to the source location. Tags are key-value pairs that
-- you can associate with Amazon resources to help with organization,
-- access control, and cost tracking. For more information, see
-- <https://docs.aws.amazon.com/mediatailor/latest/ug/tagging.html Tagging AWS Elemental MediaTailor Resources>.
sourceLocation_tags :: Lens.Lens' SourceLocation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sourceLocation_tags = Lens.lens (\SourceLocation' {tags} -> tags) (\s@SourceLocation' {} a -> s {tags = a} :: SourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | The segment delivery configurations for the source location.
sourceLocation_segmentDeliveryConfigurations :: Lens.Lens' SourceLocation (Prelude.Maybe [SegmentDeliveryConfiguration])
sourceLocation_segmentDeliveryConfigurations = Lens.lens (\SourceLocation' {segmentDeliveryConfigurations} -> segmentDeliveryConfigurations) (\s@SourceLocation' {} a -> s {segmentDeliveryConfigurations = a} :: SourceLocation) Prelude.. Lens.mapping Lens.coerced

-- | The access configuration for the source location.
sourceLocation_accessConfiguration :: Lens.Lens' SourceLocation (Prelude.Maybe AccessConfiguration)
sourceLocation_accessConfiguration = Lens.lens (\SourceLocation' {accessConfiguration} -> accessConfiguration) (\s@SourceLocation' {} a -> s {accessConfiguration = a} :: SourceLocation)

-- | The default segment delivery configuration.
sourceLocation_defaultSegmentDeliveryConfiguration :: Lens.Lens' SourceLocation (Prelude.Maybe DefaultSegmentDeliveryConfiguration)
sourceLocation_defaultSegmentDeliveryConfiguration = Lens.lens (\SourceLocation' {defaultSegmentDeliveryConfiguration} -> defaultSegmentDeliveryConfiguration) (\s@SourceLocation' {} a -> s {defaultSegmentDeliveryConfiguration = a} :: SourceLocation)

-- | The timestamp that indicates when the source location was last modified.
sourceLocation_lastModifiedTime :: Lens.Lens' SourceLocation (Prelude.Maybe Prelude.UTCTime)
sourceLocation_lastModifiedTime = Lens.lens (\SourceLocation' {lastModifiedTime} -> lastModifiedTime) (\s@SourceLocation' {} a -> s {lastModifiedTime = a} :: SourceLocation) Prelude.. Lens.mapping Data._Time

-- | The timestamp that indicates when the source location was created.
sourceLocation_creationTime :: Lens.Lens' SourceLocation (Prelude.Maybe Prelude.UTCTime)
sourceLocation_creationTime = Lens.lens (\SourceLocation' {creationTime} -> creationTime) (\s@SourceLocation' {} a -> s {creationTime = a} :: SourceLocation) Prelude.. Lens.mapping Data._Time

-- | The ARN of the SourceLocation.
sourceLocation_arn :: Lens.Lens' SourceLocation Prelude.Text
sourceLocation_arn = Lens.lens (\SourceLocation' {arn} -> arn) (\s@SourceLocation' {} a -> s {arn = a} :: SourceLocation)

-- | The HTTP configuration for the source location.
sourceLocation_httpConfiguration :: Lens.Lens' SourceLocation HttpConfiguration
sourceLocation_httpConfiguration = Lens.lens (\SourceLocation' {httpConfiguration} -> httpConfiguration) (\s@SourceLocation' {} a -> s {httpConfiguration = a} :: SourceLocation)

-- | The name of the source location.
sourceLocation_sourceLocationName :: Lens.Lens' SourceLocation Prelude.Text
sourceLocation_sourceLocationName = Lens.lens (\SourceLocation' {sourceLocationName} -> sourceLocationName) (\s@SourceLocation' {} a -> s {sourceLocationName = a} :: SourceLocation)

instance Data.FromJSON SourceLocation where
  parseJSON =
    Data.withObject
      "SourceLocation"
      ( \x ->
          SourceLocation'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "SegmentDeliveryConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AccessConfiguration")
            Prelude.<*> (x Data..:? "DefaultSegmentDeliveryConfiguration")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "HttpConfiguration")
            Prelude.<*> (x Data..: "SourceLocationName")
      )

instance Prelude.Hashable SourceLocation where
  hashWithSalt _salt SourceLocation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` segmentDeliveryConfigurations
      `Prelude.hashWithSalt` accessConfiguration
      `Prelude.hashWithSalt` defaultSegmentDeliveryConfiguration
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` httpConfiguration
      `Prelude.hashWithSalt` sourceLocationName

instance Prelude.NFData SourceLocation where
  rnf SourceLocation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf segmentDeliveryConfigurations
      `Prelude.seq` Prelude.rnf accessConfiguration
      `Prelude.seq` Prelude.rnf defaultSegmentDeliveryConfiguration
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpConfiguration
      `Prelude.seq` Prelude.rnf sourceLocationName
