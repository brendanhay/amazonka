{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisStreamConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisStreamConfig
  ( KinesisStreamConfig (..),

    -- * Smart constructor
    mkKinesisStreamConfig,

    -- * Lenses
    kscStreamARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information of a Kinesis data stream.
--
-- /See:/ 'mkKinesisStreamConfig' smart constructor.
newtype KinesisStreamConfig = KinesisStreamConfig'
  { -- | The Amazon Resource Name (ARN) of the data stream.
    streamARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisStreamConfig' with the minimum fields required to make a request.
--
-- * 'streamARN' - The Amazon Resource Name (ARN) of the data stream.
mkKinesisStreamConfig ::
  -- | 'streamARN'
  Lude.Text ->
  KinesisStreamConfig
mkKinesisStreamConfig pStreamARN_ =
  KinesisStreamConfig' {streamARN = pStreamARN_}

-- | The Amazon Resource Name (ARN) of the data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kscStreamARN :: Lens.Lens' KinesisStreamConfig Lude.Text
kscStreamARN = Lens.lens (streamARN :: KinesisStreamConfig -> Lude.Text) (\s a -> s {streamARN = a} :: KinesisStreamConfig)
{-# DEPRECATED kscStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

instance Lude.FromJSON KinesisStreamConfig where
  parseJSON =
    Lude.withObject
      "KinesisStreamConfig"
      (\x -> KinesisStreamConfig' Lude.<$> (x Lude..: "StreamArn"))

instance Lude.ToJSON KinesisStreamConfig where
  toJSON KinesisStreamConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StreamArn" Lude..= streamARN)])
