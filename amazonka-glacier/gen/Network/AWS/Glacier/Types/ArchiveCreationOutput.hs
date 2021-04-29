{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glacier.Types.ArchiveCreationOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ArchiveCreationOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the Amazon S3 Glacier response to your request.
--
-- For information about the underlying REST API, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive>.
-- For conceptual information, see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier>.
--
-- /See:/ 'newArchiveCreationOutput' smart constructor.
data ArchiveCreationOutput = ArchiveCreationOutput'
  { -- | The ID of the archive. This value is also included as part of the
    -- location.
    archiveId :: Prelude.Maybe Prelude.Text,
    -- | The relative URI path of the newly added archive resource.
    location :: Prelude.Maybe Prelude.Text,
    -- | The checksum of the archive computed by Amazon S3 Glacier.
    checksum :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ArchiveCreationOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'archiveId', 'archiveCreationOutput_archiveId' - The ID of the archive. This value is also included as part of the
-- location.
--
-- 'location', 'archiveCreationOutput_location' - The relative URI path of the newly added archive resource.
--
-- 'checksum', 'archiveCreationOutput_checksum' - The checksum of the archive computed by Amazon S3 Glacier.
newArchiveCreationOutput ::
  ArchiveCreationOutput
newArchiveCreationOutput =
  ArchiveCreationOutput'
    { archiveId = Prelude.Nothing,
      location = Prelude.Nothing,
      checksum = Prelude.Nothing
    }

-- | The ID of the archive. This value is also included as part of the
-- location.
archiveCreationOutput_archiveId :: Lens.Lens' ArchiveCreationOutput (Prelude.Maybe Prelude.Text)
archiveCreationOutput_archiveId = Lens.lens (\ArchiveCreationOutput' {archiveId} -> archiveId) (\s@ArchiveCreationOutput' {} a -> s {archiveId = a} :: ArchiveCreationOutput)

-- | The relative URI path of the newly added archive resource.
archiveCreationOutput_location :: Lens.Lens' ArchiveCreationOutput (Prelude.Maybe Prelude.Text)
archiveCreationOutput_location = Lens.lens (\ArchiveCreationOutput' {location} -> location) (\s@ArchiveCreationOutput' {} a -> s {location = a} :: ArchiveCreationOutput)

-- | The checksum of the archive computed by Amazon S3 Glacier.
archiveCreationOutput_checksum :: Lens.Lens' ArchiveCreationOutput (Prelude.Maybe Prelude.Text)
archiveCreationOutput_checksum = Lens.lens (\ArchiveCreationOutput' {checksum} -> checksum) (\s@ArchiveCreationOutput' {} a -> s {checksum = a} :: ArchiveCreationOutput)

instance Prelude.FromJSON ArchiveCreationOutput where
  parseJSON =
    Prelude.withObject
      "ArchiveCreationOutput"
      ( \x ->
          ArchiveCreationOutput'
            Prelude.<$> (x Prelude..:? "x-amz-archive-id")
            Prelude.<*> (x Prelude..:? "Location")
            Prelude.<*> (x Prelude..:? "x-amz-sha256-tree-hash")
      )

instance Prelude.Hashable ArchiveCreationOutput

instance Prelude.NFData ArchiveCreationOutput
