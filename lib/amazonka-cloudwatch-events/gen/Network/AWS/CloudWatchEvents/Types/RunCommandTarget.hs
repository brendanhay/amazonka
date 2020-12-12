{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RunCommandTarget
  ( RunCommandTarget (..),

    -- * Smart constructor
    mkRunCommandTarget,

    -- * Lenses
    rctKey,
    rctValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each @RunCommandTarget@ block can include only one key, but this key may specify multiple values.
--
-- /See:/ 'mkRunCommandTarget' smart constructor.
data RunCommandTarget = RunCommandTarget'
  { key :: Lude.Text,
    values :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunCommandTarget' with the minimum fields required to make a request.
--
-- * 'key' - Can be either @tag:@ /tag-key/ or @InstanceIds@ .
-- * 'values' - If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
mkRunCommandTarget ::
  -- | 'key'
  Lude.Text ->
  -- | 'values'
  Lude.NonEmpty Lude.Text ->
  RunCommandTarget
mkRunCommandTarget pKey_ pValues_ =
  RunCommandTarget' {key = pKey_, values = pValues_}

-- | Can be either @tag:@ /tag-key/ or @InstanceIds@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctKey :: Lens.Lens' RunCommandTarget Lude.Text
rctKey = Lens.lens (key :: RunCommandTarget -> Lude.Text) (\s a -> s {key = a} :: RunCommandTarget)
{-# DEPRECATED rctKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctValues :: Lens.Lens' RunCommandTarget (Lude.NonEmpty Lude.Text)
rctValues = Lens.lens (values :: RunCommandTarget -> Lude.NonEmpty Lude.Text) (\s a -> s {values = a} :: RunCommandTarget)
{-# DEPRECATED rctValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromJSON RunCommandTarget where
  parseJSON =
    Lude.withObject
      "RunCommandTarget"
      ( \x ->
          RunCommandTarget'
            Lude.<$> (x Lude..: "Key") Lude.<*> (x Lude..: "Values")
      )

instance Lude.ToJSON RunCommandTarget where
  toJSON RunCommandTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Key" Lude..= key),
            Lude.Just ("Values" Lude..= values)
          ]
      )
