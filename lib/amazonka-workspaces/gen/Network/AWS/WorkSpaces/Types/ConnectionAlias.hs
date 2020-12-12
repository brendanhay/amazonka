{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAlias
  ( ConnectionAlias (..),

    -- * Smart constructor
    mkConnectionAlias,

    -- * Lenses
    caState,
    caOwnerAccountId,
    caAliasId,
    caAssociations,
    caConnectionString,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
import Network.AWS.WorkSpaces.Types.ConnectionAliasState

-- | Describes a connection alias. Connection aliases are used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /See:/ 'mkConnectionAlias' smart constructor.
data ConnectionAlias = ConnectionAlias'
  { state ::
      Lude.Maybe ConnectionAliasState,
    ownerAccountId :: Lude.Maybe Lude.Text,
    aliasId :: Lude.Maybe Lude.Text,
    associations ::
      Lude.Maybe (Lude.NonEmpty ConnectionAliasAssociation),
    connectionString :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionAlias' with the minimum fields required to make a request.
--
-- * 'aliasId' - The identifier of the connection alias.
-- * 'associations' - The association status of the connection alias.
-- * 'connectionString' - The connection string specified for the connection alias. The connection string must be in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
-- * 'ownerAccountId' - The identifier of the AWS account that owns the connection alias.
-- * 'state' - The current state of the connection alias.
mkConnectionAlias ::
  ConnectionAlias
mkConnectionAlias =
  ConnectionAlias'
    { state = Lude.Nothing,
      ownerAccountId = Lude.Nothing,
      aliasId = Lude.Nothing,
      associations = Lude.Nothing,
      connectionString = Lude.Nothing
    }

-- | The current state of the connection alias.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caState :: Lens.Lens' ConnectionAlias (Lude.Maybe ConnectionAliasState)
caState = Lens.lens (state :: ConnectionAlias -> Lude.Maybe ConnectionAliasState) (\s a -> s {state = a} :: ConnectionAlias)
{-# DEPRECATED caState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The identifier of the AWS account that owns the connection alias.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOwnerAccountId :: Lens.Lens' ConnectionAlias (Lude.Maybe Lude.Text)
caOwnerAccountId = Lens.lens (ownerAccountId :: ConnectionAlias -> Lude.Maybe Lude.Text) (\s a -> s {ownerAccountId = a} :: ConnectionAlias)
{-# DEPRECATED caOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAliasId :: Lens.Lens' ConnectionAlias (Lude.Maybe Lude.Text)
caAliasId = Lens.lens (aliasId :: ConnectionAlias -> Lude.Maybe Lude.Text) (\s a -> s {aliasId = a} :: ConnectionAlias)
{-# DEPRECATED caAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The association status of the connection alias.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAssociations :: Lens.Lens' ConnectionAlias (Lude.Maybe (Lude.NonEmpty ConnectionAliasAssociation))
caAssociations = Lens.lens (associations :: ConnectionAlias -> Lude.Maybe (Lude.NonEmpty ConnectionAliasAssociation)) (\s a -> s {associations = a} :: ConnectionAlias)
{-# DEPRECATED caAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The connection string specified for the connection alias. The connection string must be in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
--
-- /Note:/ Consider using 'connectionString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caConnectionString :: Lens.Lens' ConnectionAlias (Lude.Maybe Lude.Text)
caConnectionString = Lens.lens (connectionString :: ConnectionAlias -> Lude.Maybe Lude.Text) (\s a -> s {connectionString = a} :: ConnectionAlias)
{-# DEPRECATED caConnectionString "Use generic-lens or generic-optics with 'connectionString' instead." #-}

instance Lude.FromJSON ConnectionAlias where
  parseJSON =
    Lude.withObject
      "ConnectionAlias"
      ( \x ->
          ConnectionAlias'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "OwnerAccountId")
            Lude.<*> (x Lude..:? "AliasId")
            Lude.<*> (x Lude..:? "Associations")
            Lude.<*> (x Lude..:? "ConnectionString")
      )
