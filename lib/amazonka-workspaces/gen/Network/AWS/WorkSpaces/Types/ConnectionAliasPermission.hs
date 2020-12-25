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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.AwsAccount as Types

-- | Describes the permissions for a connection alias. Connection aliases are used for cross-Region redirection. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces> .
--
-- /See:/ 'mkConnectionAliasPermission' smart constructor.
data ConnectionAliasPermission = ConnectionAliasPermission'
  { -- | The identifier of the AWS account that the connection alias is shared with.
    sharedAccountId :: Types.AwsAccount,
    -- | Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
    allowAssociation :: Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionAliasPermission' value with any optional fields omitted.
mkConnectionAliasPermission ::
  -- | 'sharedAccountId'
  Types.AwsAccount ->
  -- | 'allowAssociation'
  Core.Bool ->
  ConnectionAliasPermission
mkConnectionAliasPermission sharedAccountId allowAssociation =
  ConnectionAliasPermission' {sharedAccountId, allowAssociation}

-- | The identifier of the AWS account that the connection alias is shared with.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capSharedAccountId :: Lens.Lens' ConnectionAliasPermission Types.AwsAccount
capSharedAccountId = Lens.field @"sharedAccountId"
{-# DEPRECATED capSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

-- | Indicates whether the specified AWS account is allowed to associate the connection alias with a directory.
--
-- /Note:/ Consider using 'allowAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
capAllowAssociation :: Lens.Lens' ConnectionAliasPermission Core.Bool
capAllowAssociation = Lens.field @"allowAssociation"
{-# DEPRECATED capAllowAssociation "Use generic-lens or generic-optics with 'allowAssociation' instead." #-}

instance Core.FromJSON ConnectionAliasPermission where
  toJSON ConnectionAliasPermission {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SharedAccountId" Core..= sharedAccountId),
            Core.Just ("AllowAssociation" Core..= allowAssociation)
          ]
      )

instance Core.FromJSON ConnectionAliasPermission where
  parseJSON =
    Core.withObject "ConnectionAliasPermission" Core.$
      \x ->
        ConnectionAliasPermission'
          Core.<$> (x Core..: "SharedAccountId")
          Core.<*> (x Core..: "AllowAssociation")
