{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
  ( KinesisFirehoseInputDescription (..),

    -- * Smart constructor
    mkKinesisFirehoseInputDescription,

    -- * Lenses
    kfidResourceARN,
    kfidRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration.
--
-- /See:/ 'mkKinesisFirehoseInputDescription' smart constructor.
data KinesisFirehoseInputDescription = KinesisFirehoseInputDescription'
  { resourceARN ::
      Lude.Maybe Lude.Text,
    roleARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisFirehoseInputDescription' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
mkKinesisFirehoseInputDescription ::
  KinesisFirehoseInputDescription
mkKinesisFirehoseInputDescription =
  KinesisFirehoseInputDescription'
    { resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfidResourceARN :: Lens.Lens' KinesisFirehoseInputDescription (Lude.Maybe Lude.Text)
kfidResourceARN = Lens.lens (resourceARN :: KinesisFirehoseInputDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: KinesisFirehoseInputDescription)
{-# DEPRECATED kfidResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfidRoleARN :: Lens.Lens' KinesisFirehoseInputDescription (Lude.Maybe Lude.Text)
kfidRoleARN = Lens.lens (roleARN :: KinesisFirehoseInputDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: KinesisFirehoseInputDescription)
{-# DEPRECATED kfidRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON KinesisFirehoseInputDescription where
  parseJSON =
    Lude.withObject
      "KinesisFirehoseInputDescription"
      ( \x ->
          KinesisFirehoseInputDescription'
            Lude.<$> (x Lude..:? "ResourceARN") Lude.<*> (x Lude..:? "RoleARN")
      )
