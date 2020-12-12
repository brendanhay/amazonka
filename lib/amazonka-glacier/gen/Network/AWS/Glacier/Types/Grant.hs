{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.Grant
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.Grant
  ( Grant (..),

    -- * Smart constructor
    mkGrant,

    -- * Lenses
    gPermission,
    gGrantee,
  )
where

import Network.AWS.Glacier.Types.Grantee
import Network.AWS.Glacier.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a grant.
--
-- /See:/ 'mkGrant' smart constructor.
data Grant = Grant'
  { permission :: Lude.Maybe Permission,
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

-- | Creates a value of 'Grant' with the minimum fields required to make a request.
--
-- * 'grantee' - The grantee.
-- * 'permission' - Specifies the permission given to the grantee.
mkGrant ::
  Grant
mkGrant = Grant' {permission = Lude.Nothing, grantee = Lude.Nothing}

-- | Specifies the permission given to the grantee.
--
-- /Note:/ Consider using 'permission' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPermission :: Lens.Lens' Grant (Lude.Maybe Permission)
gPermission = Lens.lens (permission :: Grant -> Lude.Maybe Permission) (\s a -> s {permission = a} :: Grant)
{-# DEPRECATED gPermission "Use generic-lens or generic-optics with 'permission' instead." #-}

-- | The grantee.
--
-- /Note:/ Consider using 'grantee' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGrantee :: Lens.Lens' Grant (Lude.Maybe Grantee)
gGrantee = Lens.lens (grantee :: Grant -> Lude.Maybe Grantee) (\s a -> s {grantee = a} :: Grant)
{-# DEPRECATED gGrantee "Use generic-lens or generic-optics with 'grantee' instead." #-}

instance Lude.FromJSON Grant where
  parseJSON =
    Lude.withObject
      "Grant"
      ( \x ->
          Grant'
            Lude.<$> (x Lude..:? "Permission") Lude.<*> (x Lude..:? "Grantee")
      )

instance Lude.ToJSON Grant where
  toJSON Grant' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Permission" Lude..=) Lude.<$> permission,
            ("Grantee" Lude..=) Lude.<$> grantee
          ]
      )
