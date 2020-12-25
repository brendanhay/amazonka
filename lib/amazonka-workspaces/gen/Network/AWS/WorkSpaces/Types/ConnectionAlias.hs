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
    caAliasId,
    caAssociations,
    caConnectionString,
    caOwnerAccountId,
    caState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.AliasId as Types
import qualified Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation as Types
import qualified Network.AWS.WorkSpaces.Types.ConnectionAliasState as Types
import qualified Network.AWS.WorkSpaces.Types.ConnectionString as Types
import qualified Network.AWS.WorkSpaces.Types.OwnerAccountId as Types

-- | Describes a connection alias. Connection aliases are used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /See:/ 'mkConnectionAlias' smart constructor.
data ConnectionAlias = ConnectionAlias'
  { -- | The identifier of the connection alias.
    aliasId :: Core.Maybe Types.AliasId,
    -- | The association status of the connection alias.
    associations :: Core.Maybe (Core.NonEmpty Types.ConnectionAliasAssociation),
    -- | The connection string specified for the connection alias. The connection string must be in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
    connectionString :: Core.Maybe Types.ConnectionString,
    -- | The identifier of the AWS account that owns the connection alias.
    ownerAccountId :: Core.Maybe Types.OwnerAccountId,
    -- | The current state of the connection alias.
    state :: Core.Maybe Types.ConnectionAliasState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionAlias' value with any optional fields omitted.
mkConnectionAlias ::
  ConnectionAlias
mkConnectionAlias =
  ConnectionAlias'
    { aliasId = Core.Nothing,
      associations = Core.Nothing,
      connectionString = Core.Nothing,
      ownerAccountId = Core.Nothing,
      state = Core.Nothing
    }

-- | The identifier of the connection alias.
--
-- /Note:/ Consider using 'aliasId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAliasId :: Lens.Lens' ConnectionAlias (Core.Maybe Types.AliasId)
caAliasId = Lens.field @"aliasId"
{-# DEPRECATED caAliasId "Use generic-lens or generic-optics with 'aliasId' instead." #-}

-- | The association status of the connection alias.
--
-- /Note:/ Consider using 'associations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAssociations :: Lens.Lens' ConnectionAlias (Core.Maybe (Core.NonEmpty Types.ConnectionAliasAssociation))
caAssociations = Lens.field @"associations"
{-# DEPRECATED caAssociations "Use generic-lens or generic-optics with 'associations' instead." #-}

-- | The connection string specified for the connection alias. The connection string must be in the form of a fully qualified domain name (FQDN), such as @www.example.com@ .
--
-- /Note:/ Consider using 'connectionString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caConnectionString :: Lens.Lens' ConnectionAlias (Core.Maybe Types.ConnectionString)
caConnectionString = Lens.field @"connectionString"
{-# DEPRECATED caConnectionString "Use generic-lens or generic-optics with 'connectionString' instead." #-}

-- | The identifier of the AWS account that owns the connection alias.
--
-- /Note:/ Consider using 'ownerAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caOwnerAccountId :: Lens.Lens' ConnectionAlias (Core.Maybe Types.OwnerAccountId)
caOwnerAccountId = Lens.field @"ownerAccountId"
{-# DEPRECATED caOwnerAccountId "Use generic-lens or generic-optics with 'ownerAccountId' instead." #-}

-- | The current state of the connection alias.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caState :: Lens.Lens' ConnectionAlias (Core.Maybe Types.ConnectionAliasState)
caState = Lens.field @"state"
{-# DEPRECATED caState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON ConnectionAlias where
  parseJSON =
    Core.withObject "ConnectionAlias" Core.$
      \x ->
        ConnectionAlias'
          Core.<$> (x Core..:? "AliasId")
          Core.<*> (x Core..:? "Associations")
          Core.<*> (x Core..:? "ConnectionString")
          Core.<*> (x Core..:? "OwnerAccountId")
          Core.<*> (x Core..:? "State")
