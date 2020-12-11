-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
  ( KinesisStreamsInputDescription (..),

    -- * Smart constructor
    mkKinesisStreamsInputDescription,

    -- * Lenses
    ksidResourceARN,
    ksidRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration.
--
-- /See:/ 'mkKinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
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

-- | Creates a value of 'KinesisStreamsInputDescription' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
mkKinesisStreamsInputDescription ::
  KinesisStreamsInputDescription
mkKinesisStreamsInputDescription =
  KinesisStreamsInputDescription'
    { resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksidResourceARN :: Lens.Lens' KinesisStreamsInputDescription (Lude.Maybe Lude.Text)
ksidResourceARN = Lens.lens (resourceARN :: KinesisStreamsInputDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: KinesisStreamsInputDescription)
{-# DEPRECATED ksidResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksidRoleARN :: Lens.Lens' KinesisStreamsInputDescription (Lude.Maybe Lude.Text)
ksidRoleARN = Lens.lens (roleARN :: KinesisStreamsInputDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: KinesisStreamsInputDescription)
{-# DEPRECATED ksidRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON KinesisStreamsInputDescription where
  parseJSON =
    Lude.withObject
      "KinesisStreamsInputDescription"
      ( \x ->
          KinesisStreamsInputDescription'
            Lude.<$> (x Lude..:? "ResourceARN") Lude.<*> (x Lude..:? "RoleARN")
      )
