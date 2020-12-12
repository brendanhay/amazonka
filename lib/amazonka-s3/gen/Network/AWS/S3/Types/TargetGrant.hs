{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.TargetGrant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TargetGrant
  ( TargetGrant (..),

    -- * Smart constructor
    mkTargetGrant,

    -- * Lenses
    tgPermission,
    tgGrantee,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.BucketLogsPermission
import Network.AWS.S3.Types.Grantee

-- | Container for granting information.
--
-- /See:/ 'mkTargetGrant' smart constructor.
data TargetGrant = TargetGrant'
  { permission ::
      Lude.Maybe BucketLogsPermission,
    grantee :: Lude.Maybe Grantee
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetGrant' with the minimum fields required to make a request.
--
-- * 'grantee' - Container for the person being granted permissions.
-- * 'permission' - Logging permissions assigned to the grantee for the bucket.
mkTargetGrant ::
  TargetGrant
mkTargetGrant =
  TargetGrant' {permission = Lude.Nothing, grantee = Lude.Nothing}

-- | Logging permissions assigned to the grantee for the bucket.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgPermission :: Lens.Lens' TargetGrant (Lude.Maybe BucketLogsPermission)
tgPermission = Lens.lens (permission :: TargetGrant -> Lude.Maybe BucketLogsPermission) (\s a -> s {permission = a} :: TargetGrant)
{-# DEPRECATED tgPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

-- | Container for the person being granted permissions.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgGrantee :: Lens.Lens' TargetGrant (Lude.Maybe Grantee)
tgGrantee = Lens.lens (grantee :: TargetGrant -> Lude.Maybe Grantee) (\s a -> s {grantee = a} :: TargetGrant)
{-# DEPRECATED tgGrantee "Use generic-lens or generic-optics with 'grantee' instead." #-}

instance Lude.FromXML TargetGrant where
  parseXML x =
    TargetGrant'
      Lude.<$> (x Lude..@? "Permission") Lude.<*> (x Lude..@? "Grantee")

instance Lude.ToXML TargetGrant where
  toXML TargetGrant' {..} =
    Lude.mconcat
      ["Permission" Lude.@= permission, "Grantee" Lude.@= grantee]
