-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.KinesisFirehoseConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.KinesisFirehoseConfig
  ( KinesisFirehoseConfig (..),

    -- * Smart constructor
    mkKinesisFirehoseConfig,

    -- * Lenses
    kfcFirehoseARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration information of a Kinesis Firehose delivery stream.
--
-- /See:/ 'mkKinesisFirehoseConfig' smart constructor.
newtype KinesisFirehoseConfig = KinesisFirehoseConfig'
  { firehoseARN ::
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

-- | Creates a value of 'KinesisFirehoseConfig' with the minimum fields required to make a request.
--
-- * 'firehoseARN' - The Amazon Resource Name (ARN) of the delivery stream.
mkKinesisFirehoseConfig ::
  -- | 'firehoseARN'
  Lude.Text ->
  KinesisFirehoseConfig
mkKinesisFirehoseConfig pFirehoseARN_ =
  KinesisFirehoseConfig' {firehoseARN = pFirehoseARN_}

-- | The Amazon Resource Name (ARN) of the delivery stream.
--
-- /Note:/ Consider using 'firehoseARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfcFirehoseARN :: Lens.Lens' KinesisFirehoseConfig Lude.Text
kfcFirehoseARN = Lens.lens (firehoseARN :: KinesisFirehoseConfig -> Lude.Text) (\s a -> s {firehoseARN = a} :: KinesisFirehoseConfig)
{-# DEPRECATED kfcFirehoseARN "Use generic-lens or generic-optics with 'firehoseARN' instead." #-}

instance Lude.FromJSON KinesisFirehoseConfig where
  parseJSON =
    Lude.withObject
      "KinesisFirehoseConfig"
      (\x -> KinesisFirehoseConfig' Lude.<$> (x Lude..: "FirehoseArn"))

instance Lude.ToJSON KinesisFirehoseConfig where
  toJSON KinesisFirehoseConfig' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("FirehoseArn" Lude..= firehoseARN)])
