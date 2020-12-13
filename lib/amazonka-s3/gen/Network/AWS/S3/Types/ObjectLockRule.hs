{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockRule
  ( ObjectLockRule (..),

    -- * Smart constructor
    mkObjectLockRule,

    -- * Lenses
    olrDefaultRetention,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.DefaultRetention

-- | The container element for an Object Lock rule.
--
-- /See:/ 'mkObjectLockRule' smart constructor.
newtype ObjectLockRule = ObjectLockRule'
  { -- | The default retention period that you want to apply to new objects placed in the specified bucket.
    defaultRetention :: Lude.Maybe DefaultRetention
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectLockRule' with the minimum fields required to make a request.
--
-- * 'defaultRetention' - The default retention period that you want to apply to new objects placed in the specified bucket.
mkObjectLockRule ::
  ObjectLockRule
mkObjectLockRule = ObjectLockRule' {defaultRetention = Lude.Nothing}

-- | The default retention period that you want to apply to new objects placed in the specified bucket.
--
-- /Note:/ Consider using 'defaultRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olrDefaultRetention :: Lens.Lens' ObjectLockRule (Lude.Maybe DefaultRetention)
olrDefaultRetention = Lens.lens (defaultRetention :: ObjectLockRule -> Lude.Maybe DefaultRetention) (\s a -> s {defaultRetention = a} :: ObjectLockRule)
{-# DEPRECATED olrDefaultRetention "Use generic-lens or generic-optics with 'defaultRetention' instead." #-}

instance Lude.FromXML ObjectLockRule where
  parseXML x =
    ObjectLockRule' Lude.<$> (x Lude..@? "DefaultRetention")

instance Lude.ToXML ObjectLockRule where
  toXML ObjectLockRule' {..} =
    Lude.mconcat ["DefaultRetention" Lude.@= defaultRetention]
