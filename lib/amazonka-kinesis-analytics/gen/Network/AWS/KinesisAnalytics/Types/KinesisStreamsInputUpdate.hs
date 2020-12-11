-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputUpdate
  ( KinesisStreamsInputUpdate (..),

    -- * Smart constructor
    mkKinesisStreamsInputUpdate,

    -- * Lenses
    ksiuRoleARNUpdate,
    ksiuResourceARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When updating application input configuration, provides information about an Amazon Kinesis stream as the streaming source.
--
-- /See:/ 'mkKinesisStreamsInputUpdate' smart constructor.
data KinesisStreamsInputUpdate = KinesisStreamsInputUpdate'
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

-- | Creates a value of 'KinesisStreamsInputUpdate' with the minimum fields required to make a request.
--
-- * 'resourceARNUpdate' - Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
-- * 'roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
mkKinesisStreamsInputUpdate ::
  KinesisStreamsInputUpdate
mkKinesisStreamsInputUpdate =
  KinesisStreamsInputUpdate'
    { roleARNUpdate = Lude.Nothing,
      resourceARNUpdate = Lude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksiuRoleARNUpdate :: Lens.Lens' KinesisStreamsInputUpdate (Lude.Maybe Lude.Text)
ksiuRoleARNUpdate = Lens.lens (roleARNUpdate :: KinesisStreamsInputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: KinesisStreamsInputUpdate)
{-# DEPRECATED ksiuRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis stream to read.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksiuResourceARNUpdate :: Lens.Lens' KinesisStreamsInputUpdate (Lude.Maybe Lude.Text)
ksiuResourceARNUpdate = Lens.lens (resourceARNUpdate :: KinesisStreamsInputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARNUpdate = a} :: KinesisStreamsInputUpdate)
{-# DEPRECATED ksiuResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

instance Lude.ToJSON KinesisStreamsInputUpdate where
  toJSON KinesisStreamsInputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Lude..=) Lude.<$> resourceARNUpdate
          ]
      )
