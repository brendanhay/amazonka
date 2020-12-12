{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.AccessControlList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.AccessControlList
  ( AccessControlList (..),

    -- * Smart constructor
    mkAccessControlList,

    -- * Lenses
    aclAllowsPublicWriteAccess,
    aclAllowsPublicReadAccess,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the current access control policies for the bucket.
--
-- /See:/ 'mkAccessControlList' smart constructor.
data AccessControlList = AccessControlList'
  { allowsPublicWriteAccess ::
      Lude.Maybe Lude.Bool,
    allowsPublicReadAccess :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AccessControlList' with the minimum fields required to make a request.
--
-- * 'allowsPublicReadAccess' - A value that indicates whether public read access for the bucket is enabled through an Access Control List (ACL).
-- * 'allowsPublicWriteAccess' - A value that indicates whether public write access for the bucket is enabled through an Access Control List (ACL).
mkAccessControlList ::
  AccessControlList
mkAccessControlList =
  AccessControlList'
    { allowsPublicWriteAccess = Lude.Nothing,
      allowsPublicReadAccess = Lude.Nothing
    }

-- | A value that indicates whether public write access for the bucket is enabled through an Access Control List (ACL).
--
-- /Note:/ Consider using 'allowsPublicWriteAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclAllowsPublicWriteAccess :: Lens.Lens' AccessControlList (Lude.Maybe Lude.Bool)
aclAllowsPublicWriteAccess = Lens.lens (allowsPublicWriteAccess :: AccessControlList -> Lude.Maybe Lude.Bool) (\s a -> s {allowsPublicWriteAccess = a} :: AccessControlList)
{-# DEPRECATED aclAllowsPublicWriteAccess "Use generic-lens or generic-optics with 'allowsPublicWriteAccess' instead." #-}

-- | A value that indicates whether public read access for the bucket is enabled through an Access Control List (ACL).
--
-- /Note:/ Consider using 'allowsPublicReadAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aclAllowsPublicReadAccess :: Lens.Lens' AccessControlList (Lude.Maybe Lude.Bool)
aclAllowsPublicReadAccess = Lens.lens (allowsPublicReadAccess :: AccessControlList -> Lude.Maybe Lude.Bool) (\s a -> s {allowsPublicReadAccess = a} :: AccessControlList)
{-# DEPRECATED aclAllowsPublicReadAccess "Use generic-lens or generic-optics with 'allowsPublicReadAccess' instead." #-}

instance Lude.FromJSON AccessControlList where
  parseJSON =
    Lude.withObject
      "AccessControlList"
      ( \x ->
          AccessControlList'
            Lude.<$> (x Lude..:? "allowsPublicWriteAccess")
            Lude.<*> (x Lude..:? "allowsPublicReadAccess")
      )
