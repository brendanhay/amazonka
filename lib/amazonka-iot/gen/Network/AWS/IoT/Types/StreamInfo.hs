-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.StreamInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.StreamInfo
  ( StreamInfo (..),

    -- * Smart constructor
    mkStreamInfo,

    -- * Lenses
    siLastUpdatedAt,
    siCreatedAt,
    siStreamVersion,
    siStreamARN,
    siFiles,
    siDescription,
    siStreamId,
    siRoleARN,
  )
where

import Network.AWS.IoT.Types.StreamFile
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a stream.
--
-- /See:/ 'mkStreamInfo' smart constructor.
data StreamInfo = StreamInfo'
  { lastUpdatedAt ::
      Lude.Maybe Lude.Timestamp,
    createdAt :: Lude.Maybe Lude.Timestamp,
    streamVersion :: Lude.Maybe Lude.Natural,
    streamARN :: Lude.Maybe Lude.Text,
    files :: Lude.Maybe (Lude.NonEmpty StreamFile),
    description :: Lude.Maybe Lude.Text,
    streamId :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StreamInfo' with the minimum fields required to make a request.
--
-- * 'createdAt' - The date when the stream was created.
-- * 'description' - The description of the stream.
-- * 'files' - The files to stream.
-- * 'lastUpdatedAt' - The date when the stream was last updated.
-- * 'roleARN' - An IAM role AWS IoT assumes to access your S3 files.
-- * 'streamARN' - The stream ARN.
-- * 'streamId' - The stream ID.
-- * 'streamVersion' - The stream version.
mkStreamInfo ::
  StreamInfo
mkStreamInfo =
  StreamInfo'
    { lastUpdatedAt = Lude.Nothing,
      createdAt = Lude.Nothing,
      streamVersion = Lude.Nothing,
      streamARN = Lude.Nothing,
      files = Lude.Nothing,
      description = Lude.Nothing,
      streamId = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The date when the stream was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siLastUpdatedAt :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Timestamp)
siLastUpdatedAt = Lens.lens (lastUpdatedAt :: StreamInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedAt = a} :: StreamInfo)
{-# DEPRECATED siLastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead." #-}

-- | The date when the stream was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siCreatedAt :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Timestamp)
siCreatedAt = Lens.lens (createdAt :: StreamInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: StreamInfo)
{-# DEPRECATED siCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The stream version.
--
-- /Note:/ Consider using 'streamVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamVersion :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Natural)
siStreamVersion = Lens.lens (streamVersion :: StreamInfo -> Lude.Maybe Lude.Natural) (\s a -> s {streamVersion = a} :: StreamInfo)
{-# DEPRECATED siStreamVersion "Use generic-lens or generic-optics with 'streamVersion' instead." #-}

-- | The stream ARN.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamARN :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siStreamARN = Lens.lens (streamARN :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: StreamInfo)
{-# DEPRECATED siStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The files to stream.
--
-- /Note:/ Consider using 'files' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siFiles :: Lens.Lens' StreamInfo (Lude.Maybe (Lude.NonEmpty StreamFile))
siFiles = Lens.lens (files :: StreamInfo -> Lude.Maybe (Lude.NonEmpty StreamFile)) (\s a -> s {files = a} :: StreamInfo)
{-# DEPRECATED siFiles "Use generic-lens or generic-optics with 'files' instead." #-}

-- | The description of the stream.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siDescription :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siDescription = Lens.lens (description :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: StreamInfo)
{-# DEPRECATED siDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The stream ID.
--
-- /Note:/ Consider using 'streamId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siStreamId :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siStreamId = Lens.lens (streamId :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {streamId = a} :: StreamInfo)
{-# DEPRECATED siStreamId "Use generic-lens or generic-optics with 'streamId' instead." #-}

-- | An IAM role AWS IoT assumes to access your S3 files.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siRoleARN :: Lens.Lens' StreamInfo (Lude.Maybe Lude.Text)
siRoleARN = Lens.lens (roleARN :: StreamInfo -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: StreamInfo)
{-# DEPRECATED siRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON StreamInfo where
  parseJSON =
    Lude.withObject
      "StreamInfo"
      ( \x ->
          StreamInfo'
            Lude.<$> (x Lude..:? "lastUpdatedAt")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "streamVersion")
            Lude.<*> (x Lude..:? "streamArn")
            Lude.<*> (x Lude..:? "files")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "streamId")
            Lude.<*> (x Lude..:? "roleArn")
      )
