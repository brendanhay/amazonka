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
    caaResourceId,
    caaAssociationStatus,
    caaConnectionIdentifier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkSpaces.Types.AssociationStatus

-- | Describes a connection alias association that is used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /See:/ 'mkConnectionAliasAssociation' smart constructor.
data ConnectionAliasAssociation = ConnectionAliasAssociation'
  { -- | The identifier of the AWS account that associated the connection alias with a directory.
    associatedAccountId :: Lude.Maybe Lude.Text,
    -- | The identifier of the directory associated with a connection alias.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The association status of the connection alias.
    associationStatus :: Lude.Maybe AssociationStatus,
    -- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
    connectionIdentifier :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionAliasAssociation' with the minimum fields required to make a request.
--
-- * 'associatedAccountId' - The identifier of the AWS account that associated the connection alias with a directory.
-- * 'resourceId' - The identifier of the directory associated with a connection alias.
-- * 'associationStatus' - The association status of the connection alias.
-- * 'connectionIdentifier' - The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
mkConnectionAliasAssociation ::
  ConnectionAliasAssociation
mkConnectionAliasAssociation =
  ConnectionAliasAssociation'
    { associatedAccountId = Lude.Nothing,
      resourceId = Lude.Nothing,
      associationStatus = Lude.Nothing,
      connectionIdentifier = Lude.Nothing
    }

-- | The identifier of the AWS account that associated the connection alias with a directory.
--
-- /Note:/ Consider using 'associatedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAssociatedAccountId :: Lens.Lens' ConnectionAliasAssociation (Lude.Maybe Lude.Text)
caaAssociatedAccountId = Lens.lens (associatedAccountId :: ConnectionAliasAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associatedAccountId = a} :: ConnectionAliasAssociation)
{-# DEPRECATED caaAssociatedAccountId "Use generic-lens or generic-optics with 'associatedAccountId' instead." #-}

-- | The identifier of the directory associated with a connection alias.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaResourceId :: Lens.Lens' ConnectionAliasAssociation (Lude.Maybe Lude.Text)
caaResourceId = Lens.lens (resourceId :: ConnectionAliasAssociation -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ConnectionAliasAssociation)
{-# DEPRECATED caaResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The association status of the connection alias.
--
-- /Note:/ Consider using 'associationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaAssociationStatus :: Lens.Lens' ConnectionAliasAssociation (Lude.Maybe AssociationStatus)
caaAssociationStatus = Lens.lens (associationStatus :: ConnectionAliasAssociation -> Lude.Maybe AssociationStatus) (\s a -> s {associationStatus = a} :: ConnectionAliasAssociation)
{-# DEPRECATED caaAssociationStatus "Use generic-lens or generic-optics with 'associationStatus' instead." #-}

-- | The identifier of the connection alias association. You use the connection identifier in the DNS TXT record when you're configuring your DNS routing policies.
--
-- /Note:/ Consider using 'connectionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caaConnectionIdentifier :: Lens.Lens' ConnectionAliasAssociation (Lude.Maybe Lude.Text)
caaConnectionIdentifier = Lens.lens (connectionIdentifier :: ConnectionAliasAssociation -> Lude.Maybe Lude.Text) (\s a -> s {connectionIdentifier = a} :: ConnectionAliasAssociation)
{-# DEPRECATED caaConnectionIdentifier "Use generic-lens or generic-optics with 'connectionIdentifier' instead." #-}

instance Lude.FromJSON ConnectionAliasAssociation where
  parseJSON =
    Lude.withObject
      "ConnectionAliasAssociation"
      ( \x ->
          ConnectionAliasAssociation'
            Lude.<$> (x Lude..:? "AssociatedAccountId")
            Lude.<*> (x Lude..:? "ResourceId")
            Lude.<*> (x Lude..:? "AssociationStatus")
            Lude.<*> (x Lude..:? "ConnectionIdentifier")
      )
