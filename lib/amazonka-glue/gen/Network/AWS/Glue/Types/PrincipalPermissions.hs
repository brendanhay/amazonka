{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PrincipalPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PrincipalPermissions
  ( PrincipalPermissions (..),

    -- * Smart constructor
    mkPrincipalPermissions,

    -- * Lenses
    ppPrincipal,
    ppPermissions,
  )
where

import Network.AWS.Glue.Types.DataLakePrincipal
import Network.AWS.Glue.Types.Permission
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Permissions granted to a principal.
--
-- /See:/ 'mkPrincipalPermissions' smart constructor.
data PrincipalPermissions = PrincipalPermissions'
  { principal ::
      Lude.Maybe DataLakePrincipal,
    permissions :: Lude.Maybe [Permission]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrincipalPermissions' with the minimum fields required to make a request.
--
-- * 'permissions' - The permissions that are granted to the principal.
-- * 'principal' - The principal who is granted permissions.
mkPrincipalPermissions ::
  PrincipalPermissions
mkPrincipalPermissions =
  PrincipalPermissions'
    { principal = Lude.Nothing,
      permissions = Lude.Nothing
    }

-- | The principal who is granted permissions.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPrincipal :: Lens.Lens' PrincipalPermissions (Lude.Maybe DataLakePrincipal)
ppPrincipal = Lens.lens (principal :: PrincipalPermissions -> Lude.Maybe DataLakePrincipal) (\s a -> s {principal = a} :: PrincipalPermissions)
{-# DEPRECATED ppPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

-- | The permissions that are granted to the principal.
--
-- /Note:/ Consider using 'permissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPermissions :: Lens.Lens' PrincipalPermissions (Lude.Maybe [Permission])
ppPermissions = Lens.lens (permissions :: PrincipalPermissions -> Lude.Maybe [Permission]) (\s a -> s {permissions = a} :: PrincipalPermissions)
{-# DEPRECATED ppPermissions "Use generic-lens or generic-optics with 'permissions' instead." #-}

instance Lude.FromJSON PrincipalPermissions where
  parseJSON =
    Lude.withObject
      "PrincipalPermissions"
      ( \x ->
          PrincipalPermissions'
            Lude.<$> (x Lude..:? "Principal")
            Lude.<*> (x Lude..:? "Permissions" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON PrincipalPermissions where
  toJSON PrincipalPermissions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Principal" Lude..=) Lude.<$> principal,
            ("Permissions" Lude..=) Lude.<$> permissions
          ]
      )
