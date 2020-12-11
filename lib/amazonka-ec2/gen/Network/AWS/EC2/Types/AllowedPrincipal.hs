-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AllowedPrincipal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AllowedPrincipal
  ( AllowedPrincipal (..),

    -- * Smart constructor
    mkAllowedPrincipal,

    -- * Lenses
    apPrincipalType,
    apPrincipal,
  )
where

import Network.AWS.EC2.Types.PrincipalType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a principal.
--
-- /See:/ 'mkAllowedPrincipal' smart constructor.
data AllowedPrincipal = AllowedPrincipal'
  { principalType ::
      Lude.Maybe PrincipalType,
    principal :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AllowedPrincipal' with the minimum fields required to make a request.
--
-- * 'principal' - The Amazon Resource Name (ARN) of the principal.
-- * 'principalType' - The type of principal.
mkAllowedPrincipal ::
  AllowedPrincipal
mkAllowedPrincipal =
  AllowedPrincipal'
    { principalType = Lude.Nothing,
      principal = Lude.Nothing
    }

-- | The type of principal.
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPrincipalType :: Lens.Lens' AllowedPrincipal (Lude.Maybe PrincipalType)
apPrincipalType = Lens.lens (principalType :: AllowedPrincipal -> Lude.Maybe PrincipalType) (\s a -> s {principalType = a} :: AllowedPrincipal)
{-# DEPRECATED apPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

-- | The Amazon Resource Name (ARN) of the principal.
--
-- /Note:/ Consider using 'principal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPrincipal :: Lens.Lens' AllowedPrincipal (Lude.Maybe Lude.Text)
apPrincipal = Lens.lens (principal :: AllowedPrincipal -> Lude.Maybe Lude.Text) (\s a -> s {principal = a} :: AllowedPrincipal)
{-# DEPRECATED apPrincipal "Use generic-lens or generic-optics with 'principal' instead." #-}

instance Lude.FromXML AllowedPrincipal where
  parseXML x =
    AllowedPrincipal'
      Lude.<$> (x Lude..@? "principalType") Lude.<*> (x Lude..@? "principal")
