{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAliasAssociation
  ( ConnectionAliasAssociation (..),

    -- * Smart constructor
    mkConnectionAliasAssociation,

    -- * Lenses
    caaAssociatedAccountId,
    caaAssociationStatus,
    caaConnectionIdentifier,
    caaResourceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.AssociationStatus as Types
import qualified Network.AWS.WorkSpaces.Types.AwsAccount as Types
import qualified Network.AWS.WorkSpaces.Types.ConnectionIdentifier as Types
import qualified Network.AWS.WorkSpaces.Types.NonEmptyString as Types

-- | Describes a connection alias association that is used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /See:/ 'mkConnectionAliasAssociation' smart constructor.
data ConnectionAliasAssociation = ConnectionAliasAssociation'
  { -- | The identifier of the AWS account that associated the connection alias with a directory.
    associatedAccountId :: Core.Maybe Types.AwsAccount,
    -- | The association status of the connection alias.
    associationStatus :: Core.Maybe Types.AssociationStatus,
    -- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
    connectionIdentifier :: Core.Maybe Types.ConnectionIdentifier,
    -- | The identifier of the directory associated with a connection alias.
    resourceId :: Core.Maybe Types.NonEmptyString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionAliasAssociation' value with any optional fields omitted.
mkConnectionAliasAssociation ::
  ConnectionAliasAssociation
mkConnectionAliasAssociation =
  ConnectionAliasAssociation'
    { associatedAccountId = Core.Nothing,
      associationStatus = Core.Nothing,
      connectionIdentifier = Core.Nothing,
      resourceId = Core.Nothing
    }

-- | The identifier of the AWS account that associated the connection alias with a directory.
--
-- /Note:/ Consider using 'associatedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAssociatedAccountId :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Types.AwsAccount)
caaAssociatedAccountId = Lens.field @"associatedAccountId"
{-# DEPRECATED caaAssociatedAccountId "Use generic-lens or generic-optics with 'associatedAccountId' instead." #-}

-- | The association status of the connection alias.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAssociationStatus :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Types.AssociationStatus)
caaAssociationStatus = Lens.field @"associationStatus"
{-# DEPRECATED caaAssociationStatus "Use generic-lens or generic-optics with 'associationStatus' instead." #-}

-- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
--
-- /Note:/ Consider using 'connectionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaConnectionIdentifier :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Types.ConnectionIdentifier)
caaConnectionIdentifier = Lens.field @"connectionIdentifier"
{-# DEPRECATED caaConnectionIdentifier "Use generic-lens or generic-optics with 'connectionIdentifier' instead." #-}

-- | The identifier of the directory associated with a connection alias.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaResourceId :: Lens.Lens' ConnectionAliasAssociation (Core.Maybe Types.NonEmptyString)
caaResourceId = Lens.field @"resourceId"
{-# DEPRECATED caaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

instance Core.FromJSON ConnectionAliasAssociation where
  parseJSON =
    Core.withObject "ConnectionAliasAssociation" Core.$
      \x ->
        ConnectionAliasAssociation'
          Core.<$> (x Core..:? "AssociatedAccountId")
          Core.<*> (x Core..:? "AssociationStatus")
          Core.<*> (x Core..:? "ConnectionIdentifier")
          Core.<*> (x Core..:? "ResourceId")
