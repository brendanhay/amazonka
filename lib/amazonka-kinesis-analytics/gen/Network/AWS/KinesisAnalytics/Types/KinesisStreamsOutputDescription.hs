{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
  ( KinesisStreamsOutputDescription (..),

    -- * Smart constructor
    mkKinesisStreamsOutputDescription,

    -- * Lenses
    ksodResourceARN,
    ksodRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | For an application output, describes the Amazon Kinesis stream configured as its destination.
--
-- /See:/ 'mkKinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { -- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisStreamsOutputDescription' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis stream.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
mkKinesisStreamsOutputDescription ::
  KinesisStreamsOutputDescription
mkKinesisStreamsOutputDescription =
  KinesisStreamsOutputDescription'
    { resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksodResourceARN :: Lens.Lens' KinesisStreamsOutputDescription (Lude.Maybe Lude.Text)
ksodResourceARN = Lens.lens (resourceARN :: KinesisStreamsOutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: KinesisStreamsOutputDescription)
{-# DEPRECATED ksodResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksodRoleARN :: Lens.Lens' KinesisStreamsOutputDescription (Lude.Maybe Lude.Text)
ksodRoleARN = Lens.lens (roleARN :: KinesisStreamsOutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: KinesisStreamsOutputDescription)
{-# DEPRECATED ksodRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON KinesisStreamsOutputDescription where
  parseJSON =
    Lude.withObject
      "KinesisStreamsOutputDescription"
      ( \x ->
          KinesisStreamsOutputDescription'
            Lude.<$> (x Lude..:? "ResourceARN") Lude.<*> (x Lude..:? "RoleARN")
      )
