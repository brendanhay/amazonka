{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamFile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamFile
  ( StreamFile (..),

    -- * Smart constructor
    mkStreamFile,

    -- * Lenses
    sfS3Location,
    sfFileId,
  )
where

import Network.AWS.IoT.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a file to stream.
--
-- /See:/ 'mkStreamFile' smart constructor.
data StreamFile = StreamFile'
  { -- | The location of the file in S3.
    s3Location :: Lude.Maybe S3Location,
    -- | The file ID.
    fileId :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamFile' with the minimum fields required to make a request.
--
-- * 's3Location' - The location of the file in S3.
-- * 'fileId' - The file ID.
mkStreamFile ::
  StreamFile
mkStreamFile =
  StreamFile' {s3Location = Lude.Nothing, fileId = Lude.Nothing}

-- | The location of the file in S3.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfS3Location :: Lens.Lens' StreamFile (Lude.Maybe S3Location)
sfS3Location = Lens.lens (s3Location :: StreamFile -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: StreamFile)
{-# DEPRECATED sfS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The file ID.
--
-- /Note:/ Consider using 'fileId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFileId :: Lens.Lens' StreamFile (Lude.Maybe Lude.Natural)
sfFileId = Lens.lens (fileId :: StreamFile -> Lude.Maybe Lude.Natural) (\s a -> s {fileId = a} :: StreamFile)
{-# DEPRECATED sfFileId "Use generic-lens or generic-optics with 'fileId' instead." #-}

instance Lude.FromJSON StreamFile where
  parseJSON =
    Lude.withObject
      "StreamFile"
      ( \x ->
          StreamFile'
            Lude.<$> (x Lude..:? "s3Location") Lude.<*> (x Lude..:? "fileId")
      )

instance Lude.ToJSON StreamFile where
  toJSON StreamFile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("s3Location" Lude..=) Lude.<$> s3Location,
            ("fileId" Lude..=) Lude.<$> fileId
          ]
      )
