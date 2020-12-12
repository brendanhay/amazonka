{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
  ( KinesisFirehoseInputUpdate (..),

    -- * Smart constructor
    mkKinesisFirehoseInputUpdate,

    -- * Lenses
    kfiuRoleARNUpdate,
    kfiuResourceARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.
--
-- /See:/ 'mkKinesisFirehoseInputUpdate' smart constructor.
data KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate'
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

-- | Creates a value of 'KinesisFirehoseInputUpdate' with the minimum fields required to make a request.
--
-- * 'resourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
-- * 'roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisFirehoseInputUpdate ::
  KinesisFirehoseInputUpdate
mkKinesisFirehoseInputUpdate =
  KinesisFirehoseInputUpdate'
    { roleARNUpdate = Lude.Nothing,
      resourceARNUpdate = Lude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiuRoleARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate (Lude.Maybe Lude.Text)
kfiuRoleARNUpdate = Lens.lens (roleARNUpdate :: KinesisFirehoseInputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: KinesisFirehoseInputUpdate)
{-# DEPRECATED kfiuRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiuResourceARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate (Lude.Maybe Lude.Text)
kfiuResourceARNUpdate = Lens.lens (resourceARNUpdate :: KinesisFirehoseInputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARNUpdate = a} :: KinesisFirehoseInputUpdate)
{-# DEPRECATED kfiuResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

instance Lude.ToJSON KinesisFirehoseInputUpdate where
  toJSON KinesisFirehoseInputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Lude..=) Lude.<$> resourceARNUpdate
          ]
      )
