{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.ArchiveCreationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.ArchiveCreationOutput
  ( ArchiveCreationOutput (..),

    -- * Smart constructor
    mkArchiveCreationOutput,

    -- * Lenses
    acoArchiveId,
    acoChecksum,
    acoLocation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the Amazon S3 Glacier response to your request.
--
-- For information about the underlying REST API, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-archive-post.html Upload Archive> . For conceptual information, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/working-with-archives.html Working with Archives in Amazon S3 Glacier> .
--
-- /See:/ 'mkArchiveCreationOutput' smart constructor.
data ArchiveCreationOutput = ArchiveCreationOutput'
  { archiveId ::
      Lude.Maybe Lude.Text,
    checksum :: Lude.Maybe Lude.Text,
    location :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArchiveCreationOutput' with the minimum fields required to make a request.
--
-- * 'archiveId' - The ID of the archive. This value is also included as part of the location.
-- * 'checksum' - The checksum of the archive computed by Amazon S3 Glacier.
-- * 'location' - The relative URI path of the newly added archive resource.
mkArchiveCreationOutput ::
  ArchiveCreationOutput
mkArchiveCreationOutput =
  ArchiveCreationOutput'
    { archiveId = Lude.Nothing,
      checksum = Lude.Nothing,
      location = Lude.Nothing
    }

-- | The ID of the archive. This value is also included as part of the location.
--
-- /Note:/ Consider using 'archiveId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoArchiveId :: Lens.Lens' ArchiveCreationOutput (Lude.Maybe Lude.Text)
acoArchiveId = Lens.lens (archiveId :: ArchiveCreationOutput -> Lude.Maybe Lude.Text) (\s a -> s {archiveId = a} :: ArchiveCreationOutput)
{-# DEPRECATED acoArchiveId "Use generic-lens or generic-optics with 'archiveId' instead." #-}

-- | The checksum of the archive computed by Amazon S3 Glacier.
--
-- /Note:/ Consider using 'checksum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoChecksum :: Lens.Lens' ArchiveCreationOutput (Lude.Maybe Lude.Text)
acoChecksum = Lens.lens (checksum :: ArchiveCreationOutput -> Lude.Maybe Lude.Text) (\s a -> s {checksum = a} :: ArchiveCreationOutput)
{-# DEPRECATED acoChecksum "Use generic-lens or generic-optics with 'checksum' instead." #-}

-- | The relative URI path of the newly added archive resource.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acoLocation :: Lens.Lens' ArchiveCreationOutput (Lude.Maybe Lude.Text)
acoLocation = Lens.lens (location :: ArchiveCreationOutput -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: ArchiveCreationOutput)
{-# DEPRECATED acoLocation "Use generic-lens or generic-optics with 'location' instead." #-}

instance Lude.FromJSON ArchiveCreationOutput where
  parseJSON =
    Lude.withObject
      "ArchiveCreationOutput"
      ( \x ->
          ArchiveCreationOutput'
            Lude.<$> (x Lude..:? "x-amz-archive-id")
            Lude.<*> (x Lude..:? "x-amz-sha256-tree-hash")
            Lude.<*> (x Lude..:? "Location")
      )
