{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.ChannelInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.ChannelInfo
  ( ChannelInfo (..),

    -- * Smart constructor
    mkChannelInfo,

    -- * Lenses
    ciCreationTime,
    ciChannelStatus,
    ciChannelARN,
    ciSingleMasterConfiguration,
    ciChannelName,
    ciVersion,
    ciChannelType,
  )
where

import Network.AWS.KinesisVideo.Types.ChannelType
import Network.AWS.KinesisVideo.Types.SingleMasterConfiguration
import Network.AWS.KinesisVideo.Types.StreamStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A structure that encapsulates a signaling channel's metadata and properties.
--
-- /See:/ 'mkChannelInfo' smart constructor.
data ChannelInfo = ChannelInfo'
  { -- | The time at which the signaling channel was created.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | Current status of the signaling channel.
    channelStatus :: Lude.Maybe StreamStatus,
    -- | The Amazon Resource Name (ARN) of the signaling channel.
    channelARN :: Lude.Maybe Lude.Text,
    -- | A structure that contains the configuration for the @SINGLE_MASTER@ channel type.
    singleMasterConfiguration :: Lude.Maybe SingleMasterConfiguration,
    -- | The name of the signaling channel.
    channelName :: Lude.Maybe Lude.Text,
    -- | The current version of the signaling channel.
    version :: Lude.Maybe Lude.Text,
    -- | The type of the signaling channel.
    channelType :: Lude.Maybe ChannelType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelInfo' with the minimum fields required to make a request.
--
-- * 'creationTime' - The time at which the signaling channel was created.
-- * 'channelStatus' - Current status of the signaling channel.
-- * 'channelARN' - The Amazon Resource Name (ARN) of the signaling channel.
-- * 'singleMasterConfiguration' - A structure that contains the configuration for the @SINGLE_MASTER@ channel type.
-- * 'channelName' - The name of the signaling channel.
-- * 'version' - The current version of the signaling channel.
-- * 'channelType' - The type of the signaling channel.
mkChannelInfo ::
  ChannelInfo
mkChannelInfo =
  ChannelInfo'
    { creationTime = Lude.Nothing,
      channelStatus = Lude.Nothing,
      channelARN = Lude.Nothing,
      singleMasterConfiguration = Lude.Nothing,
      channelName = Lude.Nothing,
      version = Lude.Nothing,
      channelType = Lude.Nothing
    }

-- | The time at which the signaling channel was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCreationTime :: Lens.Lens' ChannelInfo (Lude.Maybe Lude.Timestamp)
ciCreationTime = Lens.lens (creationTime :: ChannelInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: ChannelInfo)
{-# DEPRECATED ciCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Current status of the signaling channel.
--
-- /Note:/ Consider using 'channelStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciChannelStatus :: Lens.Lens' ChannelInfo (Lude.Maybe StreamStatus)
ciChannelStatus = Lens.lens (channelStatus :: ChannelInfo -> Lude.Maybe StreamStatus) (\s a -> s {channelStatus = a} :: ChannelInfo)
{-# DEPRECATED ciChannelStatus "Use generic-lens or generic-optics with 'channelStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the signaling channel.
--
-- /Note:/ Consider using 'channelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciChannelARN :: Lens.Lens' ChannelInfo (Lude.Maybe Lude.Text)
ciChannelARN = Lens.lens (channelARN :: ChannelInfo -> Lude.Maybe Lude.Text) (\s a -> s {channelARN = a} :: ChannelInfo)
{-# DEPRECATED ciChannelARN "Use generic-lens or generic-optics with 'channelARN' instead." #-}

-- | A structure that contains the configuration for the @SINGLE_MASTER@ channel type.
--
-- /Note:/ Consider using 'singleMasterConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciSingleMasterConfiguration :: Lens.Lens' ChannelInfo (Lude.Maybe SingleMasterConfiguration)
ciSingleMasterConfiguration = Lens.lens (singleMasterConfiguration :: ChannelInfo -> Lude.Maybe SingleMasterConfiguration) (\s a -> s {singleMasterConfiguration = a} :: ChannelInfo)
{-# DEPRECATED ciSingleMasterConfiguration "Use generic-lens or generic-optics with 'singleMasterConfiguration' instead." #-}

-- | The name of the signaling channel.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciChannelName :: Lens.Lens' ChannelInfo (Lude.Maybe Lude.Text)
ciChannelName = Lens.lens (channelName :: ChannelInfo -> Lude.Maybe Lude.Text) (\s a -> s {channelName = a} :: ChannelInfo)
{-# DEPRECATED ciChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The current version of the signaling channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciVersion :: Lens.Lens' ChannelInfo (Lude.Maybe Lude.Text)
ciVersion = Lens.lens (version :: ChannelInfo -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: ChannelInfo)
{-# DEPRECATED ciVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The type of the signaling channel.
--
-- /Note:/ Consider using 'channelType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciChannelType :: Lens.Lens' ChannelInfo (Lude.Maybe ChannelType)
ciChannelType = Lens.lens (channelType :: ChannelInfo -> Lude.Maybe ChannelType) (\s a -> s {channelType = a} :: ChannelInfo)
{-# DEPRECATED ciChannelType "Use generic-lens or generic-optics with 'channelType' instead." #-}

instance Lude.FromJSON ChannelInfo where
  parseJSON =
    Lude.withObject
      "ChannelInfo"
      ( \x ->
          ChannelInfo'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "ChannelStatus")
            Lude.<*> (x Lude..:? "ChannelARN")
            Lude.<*> (x Lude..:? "SingleMasterConfiguration")
            Lude.<*> (x Lude..:? "ChannelName")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "ChannelType")
      )
