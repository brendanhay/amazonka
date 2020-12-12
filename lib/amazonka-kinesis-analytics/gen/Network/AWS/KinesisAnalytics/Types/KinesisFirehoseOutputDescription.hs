{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputDescription
  ( KinesisFirehoseOutputDescription (..),

    -- * Smart constructor
    mkKinesisFirehoseOutputDescription,

    -- * Lenses
    kfodResourceARN,
    kfodRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | For an application output, describes the Amazon Kinesis Firehose delivery stream configured as its destination.
--
-- /See:/ 'mkKinesisFirehoseOutputDescription' smart constructor.
data KinesisFirehoseOutputDescription = KinesisFirehoseOutputDescription'
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

-- | Creates a value of 'KinesisFirehoseOutputDescription' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
mkKinesisFirehoseOutputDescription ::
  KinesisFirehoseOutputDescription
mkKinesisFirehoseOutputDescription =
  KinesisFirehoseOutputDescription'
    { resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfodResourceARN :: Lens.Lens' KinesisFirehoseOutputDescription (Lude.Maybe Lude.Text)
kfodResourceARN = Lens.lens (resourceARN :: KinesisFirehoseOutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: KinesisFirehoseOutputDescription)
{-# DEPRECATED kfodResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfodRoleARN :: Lens.Lens' KinesisFirehoseOutputDescription (Lude.Maybe Lude.Text)
kfodRoleARN = Lens.lens (roleARN :: KinesisFirehoseOutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: KinesisFirehoseOutputDescription)
{-# DEPRECATED kfodRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON KinesisFirehoseOutputDescription where
  parseJSON =
    Lude.withObject
      "KinesisFirehoseOutputDescription"
      ( \x ->
          KinesisFirehoseOutputDescription'
            Lude.<$> (x Lude..:? "ResourceARN") Lude.<*> (x Lude..:? "RoleARN")
      )
