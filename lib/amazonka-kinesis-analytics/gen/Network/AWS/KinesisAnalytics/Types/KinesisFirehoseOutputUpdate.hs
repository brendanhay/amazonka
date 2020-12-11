-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
  ( KinesisFirehoseOutputUpdate (..),

    -- * Smart constructor
    mkKinesisFirehoseOutputUpdate,

    -- * Lenses
    kfouRoleARNUpdate,
    kfouResourceARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination.
--
-- /See:/ 'mkKinesisFirehoseOutputUpdate' smart constructor.
data KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate'
  { roleARNUpdate ::
      Lude.Maybe Lude.Text,
    resourceARNUpdate ::
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

-- | Creates a value of 'KinesisFirehoseOutputUpdate' with the minimum fields required to make a request.
--
-- * 'resourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
-- * 'roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisFirehoseOutputUpdate ::
  KinesisFirehoseOutputUpdate
mkKinesisFirehoseOutputUpdate =
  KinesisFirehoseOutputUpdate'
    { roleARNUpdate = Lude.Nothing,
      resourceARNUpdate = Lude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfouRoleARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate (Lude.Maybe Lude.Text)
kfouRoleARNUpdate = Lens.lens (roleARNUpdate :: KinesisFirehoseOutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: KinesisFirehoseOutputUpdate)
{-# DEPRECATED kfouRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfouResourceARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate (Lude.Maybe Lude.Text)
kfouResourceARNUpdate = Lens.lens (resourceARNUpdate :: KinesisFirehoseOutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARNUpdate = a} :: KinesisFirehoseOutputUpdate)
{-# DEPRECATED kfouResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

instance Lude.ToJSON KinesisFirehoseOutputUpdate where
  toJSON KinesisFirehoseOutputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Lude..=) Lude.<$> resourceARNUpdate
          ]
      )
