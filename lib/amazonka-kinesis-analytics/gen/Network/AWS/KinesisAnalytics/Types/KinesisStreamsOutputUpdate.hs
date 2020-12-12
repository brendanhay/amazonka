{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputUpdate
  ( KinesisStreamsOutputUpdate (..),

    -- * Smart constructor
    mkKinesisStreamsOutputUpdate,

    -- * Lenses
    ksouRoleARNUpdate,
    ksouResourceARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an Amazon Kinesis stream configured as the destination.
--
-- /See:/ 'mkKinesisStreamsOutputUpdate' smart constructor.
data KinesisStreamsOutputUpdate = KinesisStreamsOutputUpdate'
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

-- | Creates a value of 'KinesisStreamsOutputUpdate' with the minimum fields required to make a request.
--
-- * 'resourceARNUpdate' - Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want to write the output.
-- * 'roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisStreamsOutputUpdate ::
  KinesisStreamsOutputUpdate
mkKinesisStreamsOutputUpdate =
  KinesisStreamsOutputUpdate'
    { roleARNUpdate = Lude.Nothing,
      resourceARNUpdate = Lude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksouRoleARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate (Lude.Maybe Lude.Text)
ksouRoleARNUpdate = Lens.lens (roleARNUpdate :: KinesisStreamsOutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: KinesisStreamsOutputUpdate)
{-# DEPRECATED ksouRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream where you want to write the output.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksouResourceARNUpdate :: Lens.Lens' KinesisStreamsOutputUpdate (Lude.Maybe Lude.Text)
ksouResourceARNUpdate = Lens.lens (resourceARNUpdate :: KinesisStreamsOutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARNUpdate = a} :: KinesisStreamsOutputUpdate)
{-# DEPRECATED ksouResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

instance Lude.ToJSON KinesisStreamsOutputUpdate where
  toJSON KinesisStreamsOutputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Lude..=) Lude.<$> resourceARNUpdate
          ]
      )
