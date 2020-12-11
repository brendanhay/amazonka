-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
  ( ResourceServerType (..),

    -- * Smart constructor
    mkResourceServerType,

    -- * Lenses
    rstUserPoolId,
    rstIdentifier,
    rstScopes,
    rstName,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for information about a resource server for a user pool.
--
-- /See:/ 'mkResourceServerType' smart constructor.
data ResourceServerType = ResourceServerType'
  { userPoolId ::
      Lude.Maybe Lude.Text,
    identifier :: Lude.Maybe Lude.Text,
    scopes :: Lude.Maybe [ResourceServerScopeType],
    name :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceServerType' with the minimum fields required to make a request.
--
-- * 'identifier' - The identifier for the resource server.
-- * 'name' - The name of the resource server.
-- * 'scopes' - A list of scopes that are defined for the resource server.
-- * 'userPoolId' - The user pool ID for the user pool that hosts the resource server.
mkResourceServerType ::
  ResourceServerType
mkResourceServerType =
  ResourceServerType'
    { userPoolId = Lude.Nothing,
      identifier = Lude.Nothing,
      scopes = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The user pool ID for the user pool that hosts the resource server.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstUserPoolId :: Lens.Lens' ResourceServerType (Lude.Maybe Lude.Text)
rstUserPoolId = Lens.lens (userPoolId :: ResourceServerType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: ResourceServerType)
{-# DEPRECATED rstUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The identifier for the resource server.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstIdentifier :: Lens.Lens' ResourceServerType (Lude.Maybe Lude.Text)
rstIdentifier = Lens.lens (identifier :: ResourceServerType -> Lude.Maybe Lude.Text) (\s a -> s {identifier = a} :: ResourceServerType)
{-# DEPRECATED rstIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | A list of scopes that are defined for the resource server.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstScopes :: Lens.Lens' ResourceServerType (Lude.Maybe [ResourceServerScopeType])
rstScopes = Lens.lens (scopes :: ResourceServerType -> Lude.Maybe [ResourceServerScopeType]) (\s a -> s {scopes = a} :: ResourceServerType)
{-# DEPRECATED rstScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

-- | The name of the resource server.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rstName :: Lens.Lens' ResourceServerType (Lude.Maybe Lude.Text)
rstName = Lens.lens (name :: ResourceServerType -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ResourceServerType)
{-# DEPRECATED rstName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ResourceServerType where
  parseJSON =
    Lude.withObject
      "ResourceServerType"
      ( \x ->
          ResourceServerType'
            Lude.<$> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "Identifier")
            Lude.<*> (x Lude..:? "Scopes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
      )
