{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Grant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Grant
  ( Grant (..),

    -- * Smart constructor
    mkGrant,

    -- * Lenses
    gPermission,
    gGrantee,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Grantee
import Network.AWS.S3.Types.Permission

-- | Container for grant information.
--
-- /See:/ 'mkGrant' smart constructor.
data Grant = Grant'
  { -- | Specifies the permission given to the grantee.
    permission :: Lude.Maybe Permission,
    -- | The person being granted permissions.
    grantee :: Lude.Maybe Grantee
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Grant' with the minimum fields required to make a request.
--
-- * 'permission' - Specifies the permission given to the grantee.
-- * 'grantee' - The person being granted permissions.
mkGrant ::
  Grant
mkGrant = Grant' {permission = Lude.Nothing, grantee = Lude.Nothing}

-- | Specifies the permission given to the grantee.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPermission :: Lens.Lens' Grant (Lude.Maybe Permission)
gPermission = Lens.lens (permission :: Grant -> Lude.Maybe Permission) (\s a -> s {permission = a} :: Grant)
{-# DEPRECATED gPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

-- | The person being granted permissions.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGrantee :: Lens.Lens' Grant (Lude.Maybe Grantee)
gGrantee = Lens.lens (grantee :: Grant -> Lude.Maybe Grantee) (\s a -> s {grantee = a} :: Grant)
{-# DEPRECATED gGrantee "Use generic-lens or generic-optics with 'grantee' instead." #-}

instance Lude.FromXML Grant where
  parseXML x =
    Grant'
      Lude.<$> (x Lude..@? "Permission") Lude.<*> (x Lude..@? "Grantee")

instance Lude.ToXML Grant where
  toXML Grant' {..} =
    Lude.mconcat
      ["Permission" Lude.@= permission, "Grantee" Lude.@= grantee]
