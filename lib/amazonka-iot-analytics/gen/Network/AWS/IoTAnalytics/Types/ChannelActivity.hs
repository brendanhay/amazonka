{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ChannelActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ChannelActivity
  ( ChannelActivity (..),

    -- * Smart constructor
    mkChannelActivity,

    -- * Lenses
    caNext,
    caChannelName,
    caName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The activity that determines the source of the messages to be processed.
--
-- /See:/ 'mkChannelActivity' smart constructor.
data ChannelActivity = ChannelActivity'
  { -- | The next activity in the pipeline.
    next :: Lude.Maybe Lude.Text,
    -- | The name of the channel from which the messages are processed.
    channelName :: Lude.Text,
    -- | The name of the channel activity.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelActivity' with the minimum fields required to make a request.
--
-- * 'next' - The next activity in the pipeline.
-- * 'channelName' - The name of the channel from which the messages are processed.
-- * 'name' - The name of the channel activity.
mkChannelActivity ::
  -- | 'channelName'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ChannelActivity
mkChannelActivity pChannelName_ pName_ =
  ChannelActivity'
    { next = Lude.Nothing,
      channelName = pChannelName_,
      name = pName_
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caNext :: Lens.Lens' ChannelActivity (Lude.Maybe Lude.Text)
caNext = Lens.lens (next :: ChannelActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: ChannelActivity)
{-# DEPRECATED caNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the channel from which the messages are processed.
--
-- /Note:/ Consider using 'channelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caChannelName :: Lens.Lens' ChannelActivity Lude.Text
caChannelName = Lens.lens (channelName :: ChannelActivity -> Lude.Text) (\s a -> s {channelName = a} :: ChannelActivity)
{-# DEPRECATED caChannelName "Use generic-lens or generic-optics with 'channelName' instead." #-}

-- | The name of the channel activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' ChannelActivity Lude.Text
caName = Lens.lens (name :: ChannelActivity -> Lude.Text) (\s a -> s {name = a} :: ChannelActivity)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ChannelActivity where
  parseJSON =
    Lude.withObject
      "ChannelActivity"
      ( \x ->
          ChannelActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "channelName")
            Lude.<*> (x Lude..: "name")
      )

instance Lude.ToJSON ChannelActivity where
  toJSON ChannelActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("channelName" Lude..= channelName),
            Lude.Just ("name" Lude..= name)
          ]
      )
