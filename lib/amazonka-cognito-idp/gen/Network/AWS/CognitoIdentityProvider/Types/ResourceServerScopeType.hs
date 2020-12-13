{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
  ( ResourceServerScopeType (..),

    -- * Smart constructor
    mkResourceServerScopeType,

    -- * Lenses
    rsstScopeName,
    rsstScopeDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A resource server scope.
--
-- /See:/ 'mkResourceServerScopeType' smart constructor.
data ResourceServerScopeType = ResourceServerScopeType'
  { -- | The name of the scope.
    scopeName :: Lude.Text,
    -- | A description of the scope.
    scopeDescription :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceServerScopeType' with the minimum fields required to make a request.
--
-- * 'scopeName' - The name of the scope.
-- * 'scopeDescription' - A description of the scope.
mkResourceServerScopeType ::
  -- | 'scopeName'
  Lude.Text ->
  -- | 'scopeDescription'
  Lude.Text ->
  ResourceServerScopeType
mkResourceServerScopeType pScopeName_ pScopeDescription_ =
  ResourceServerScopeType'
    { scopeName = pScopeName_,
      scopeDescription = pScopeDescription_
    }

-- | The name of the scope.
--
-- /Note:/ Consider using 'scopeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsstScopeName :: Lens.Lens' ResourceServerScopeType Lude.Text
rsstScopeName = Lens.lens (scopeName :: ResourceServerScopeType -> Lude.Text) (\s a -> s {scopeName = a} :: ResourceServerScopeType)
{-# DEPRECATED rsstScopeName "Use generic-lens or generic-optics with 'scopeName' instead." #-}

-- | A description of the scope.
--
-- /Note:/ Consider using 'scopeDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsstScopeDescription :: Lens.Lens' ResourceServerScopeType Lude.Text
rsstScopeDescription = Lens.lens (scopeDescription :: ResourceServerScopeType -> Lude.Text) (\s a -> s {scopeDescription = a} :: ResourceServerScopeType)
{-# DEPRECATED rsstScopeDescription "Use generic-lens or generic-optics with 'scopeDescription' instead." #-}

instance Lude.FromJSON ResourceServerScopeType where
  parseJSON =
    Lude.withObject
      "ResourceServerScopeType"
      ( \x ->
          ResourceServerScopeType'
            Lude.<$> (x Lude..: "ScopeName") Lude.<*> (x Lude..: "ScopeDescription")
      )

instance Lude.ToJSON ResourceServerScopeType where
  toJSON ResourceServerScopeType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ScopeName" Lude..= scopeName),
            Lude.Just ("ScopeDescription" Lude..= scopeDescription)
          ]
      )
