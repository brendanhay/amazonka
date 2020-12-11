-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.Principal
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.Principal
  ( Principal (..),

    -- * Smart constructor
    mkPrincipal,

    -- * Lenses
    pPrincipalType,
    pPrincipalARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServiceCatalog.Types.PrincipalType

-- | Information about a principal.
--
-- /See:/ 'mkPrincipal' smart constructor.
data Principal = Principal'
  { principalType ::
      Lude.Maybe PrincipalType,
    principalARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Principal' with the minimum fields required to make a request.
--
-- * 'principalARN' - The ARN of the principal (IAM user, role, or group).
-- * 'principalType' - The principal type. The supported value is @IAM@ .
mkPrincipal ::
  Principal
mkPrincipal =
  Principal'
    { principalType = Lude.Nothing,
      principalARN = Lude.Nothing
    }

-- | The principal type. The supported value is @IAM@ .
--
-- /Note:/ Consider using 'principalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPrincipalType :: Lens.Lens' Principal (Lude.Maybe PrincipalType)
pPrincipalType = Lens.lens (principalType :: Principal -> Lude.Maybe PrincipalType) (\s a -> s {principalType = a} :: Principal)
{-# DEPRECATED pPrincipalType "Use generic-lens or generic-optics with 'principalType' instead." #-}

-- | The ARN of the principal (IAM user, role, or group).
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pPrincipalARN :: Lens.Lens' Principal (Lude.Maybe Lude.Text)
pPrincipalARN = Lens.lens (principalARN :: Principal -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: Principal)
{-# DEPRECATED pPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

instance Lude.FromJSON Principal where
  parseJSON =
    Lude.withObject
      "Principal"
      ( \x ->
          Principal'
            Lude.<$> (x Lude..:? "PrincipalType") Lude.<*> (x Lude..:? "PrincipalARN")
      )
