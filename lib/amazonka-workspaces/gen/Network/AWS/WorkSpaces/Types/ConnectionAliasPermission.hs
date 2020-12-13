{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionAliasPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionAliasPermission
  ( ConnectionAliasPermission (..),

    -- * Smart constructor
    mkConnectionAliasPermission,

    -- * Lenses
    capSharedAccountId,
    capAllowAssociation,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the permissions for a connection alias. Connection aliases are used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /See:/ 'mkConnectionAliasPermission' smart constructor.
data ConnectionAliasPermission = ConnectionAliasPermission'
  { -- | The identifier of the AWS account that the connection alias is shared with.
    sharedAccountId :: Lude.Text,
    -- | Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
    allowAssociation :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConnectionAliasPermission' with the minimum fields required to make a request.
--
-- * 'sharedAccountId' - The identifier of the AWS account that the connection alias is shared with.
-- * 'allowAssociation' - Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
mkConnectionAliasPermission ::
  -- | 'sharedAccountId'
  Lude.Text ->
  -- | 'allowAssociation'
  Lude.Bool ->
  ConnectionAliasPermission
mkConnectionAliasPermission pSharedAccountId_ pAllowAssociation_ =
  ConnectionAliasPermission'
    { sharedAccountId = pSharedAccountId_,
      allowAssociation = pAllowAssociation_
    }

-- | The identifier of the AWS account that the connection alias is shared with.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capSharedAccountId :: Lens.Lens' ConnectionAliasPermission Lude.Text
capSharedAccountId = Lens.lens (sharedAccountId :: ConnectionAliasPermission -> Lude.Text) (\s a -> s {sharedAccountId = a} :: ConnectionAliasPermission)
{-# DEPRECATED capSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
--
-- /Note:/ Consider using 'allowAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capAllowAssociation :: Lens.Lens' ConnectionAliasPermission Lude.Bool
capAllowAssociation = Lens.lens (allowAssociation :: ConnectionAliasPermission -> Lude.Bool) (\s a -> s {allowAssociation = a} :: ConnectionAliasPermission)
{-# DEPRECATED capAllowAssociation "Use generic-lens or generic-optics with 'allowAssociation' instead." #-}

instance Lude.FromJSON ConnectionAliasPermission where
  parseJSON =
    Lude.withObject
      "ConnectionAliasPermission"
      ( \x ->
          ConnectionAliasPermission'
            Lude.<$> (x Lude..: "SharedAccountId")
            Lude.<*> (x Lude..: "AllowAssociation")
      )

instance Lude.ToJSON ConnectionAliasPermission where
  toJSON ConnectionAliasPermission' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SharedAccountId" Lude..= sharedAccountId),
            Lude.Just ("AllowAssociation" Lude..= allowAssociation)
          ]
      )
