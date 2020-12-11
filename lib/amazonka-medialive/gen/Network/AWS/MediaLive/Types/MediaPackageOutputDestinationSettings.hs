-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MediaPackageOutputDestinationSettings
  ( MediaPackageOutputDestinationSettings (..),

    -- * Smart constructor
    mkMediaPackageOutputDestinationSettings,

    -- * Lenses
    mpodsChannelId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | MediaPackage Output Destination Settings
--
-- /See:/ 'mkMediaPackageOutputDestinationSettings' smart constructor.
newtype MediaPackageOutputDestinationSettings = MediaPackageOutputDestinationSettings'
  { channelId ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MediaPackageOutputDestinationSettings' with the minimum fields required to make a request.
--
-- * 'channelId' - ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
mkMediaPackageOutputDestinationSettings ::
  MediaPackageOutputDestinationSettings
mkMediaPackageOutputDestinationSettings =
  MediaPackageOutputDestinationSettings' {channelId = Lude.Nothing}

-- | ID of the channel in MediaPackage that is the destination for this output group. You do not need to specify the individual inputs in MediaPackage; MediaLive will handle the connection of the two MediaLive pipelines to the two MediaPackage inputs. The MediaPackage channel and MediaLive channel must be in the same region.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mpodsChannelId :: Lens.Lens' MediaPackageOutputDestinationSettings (Lude.Maybe Lude.Text)
mpodsChannelId = Lens.lens (channelId :: MediaPackageOutputDestinationSettings -> Lude.Maybe Lude.Text) (\s a -> s {channelId = a} :: MediaPackageOutputDestinationSettings)
{-# DEPRECATED mpodsChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

instance Lude.FromJSON MediaPackageOutputDestinationSettings where
  parseJSON =
    Lude.withObject
      "MediaPackageOutputDestinationSettings"
      ( \x ->
          MediaPackageOutputDestinationSettings'
            Lude.<$> (x Lude..:? "channelId")
      )

instance Lude.ToJSON MediaPackageOutputDestinationSettings where
  toJSON MediaPackageOutputDestinationSettings' {..} =
    Lude.object
      (Lude.catMaybes [("channelId" Lude..=) Lude.<$> channelId])
