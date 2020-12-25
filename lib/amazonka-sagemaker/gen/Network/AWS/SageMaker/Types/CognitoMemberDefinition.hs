{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CognitoMemberDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CognitoMemberDefinition
  ( CognitoMemberDefinition (..),

    -- * Smart constructor
    mkCognitoMemberDefinition,

    -- * Lenses
    cmdUserPool,
    cmdUserGroup,
    cmdClientId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ClientId as Types
import qualified Network.AWS.SageMaker.Types.UserGroup as Types
import qualified Network.AWS.SageMaker.Types.UserPool as Types

-- | Identifies a Amazon Cognito user group. A user group can be used in on or more work teams.
--
-- /See:/ 'mkCognitoMemberDefinition' smart constructor.
data CognitoMemberDefinition = CognitoMemberDefinition'
  { -- | An identifier for a user pool. The user pool must be in the same region as the service that you are calling.
    userPool :: Types.UserPool,
    -- | An identifier for a user group.
    userGroup :: Types.UserGroup,
    -- | An identifier for an application client. You must create the app client ID using Amazon Cognito.
    clientId :: Types.ClientId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CognitoMemberDefinition' value with any optional fields omitted.
mkCognitoMemberDefinition ::
  -- | 'userPool'
  Types.UserPool ->
  -- | 'userGroup'
  Types.UserGroup ->
  -- | 'clientId'
  Types.ClientId ->
  CognitoMemberDefinition
mkCognitoMemberDefinition userPool userGroup clientId =
  CognitoMemberDefinition' {userPool, userGroup, clientId}

-- | An identifier for a user pool. The user pool must be in the same region as the service that you are calling.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdUserPool :: Lens.Lens' CognitoMemberDefinition Types.UserPool
cmdUserPool = Lens.field @"userPool"
{-# DEPRECATED cmdUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

-- | An identifier for a user group.
--
-- /Note:/ Consider using 'userGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdUserGroup :: Lens.Lens' CognitoMemberDefinition Types.UserGroup
cmdUserGroup = Lens.field @"userGroup"
{-# DEPRECATED cmdUserGroup "Use generic-lens or generic-optics with 'userGroup' instead." #-}

-- | An identifier for an application client. You must create the app client ID using Amazon Cognito.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmdClientId :: Lens.Lens' CognitoMemberDefinition Types.ClientId
cmdClientId = Lens.field @"clientId"
{-# DEPRECATED cmdClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

instance Core.FromJSON CognitoMemberDefinition where
  toJSON CognitoMemberDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPool" Core..= userPool),
            Core.Just ("UserGroup" Core..= userGroup),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.FromJSON CognitoMemberDefinition where
  parseJSON =
    Core.withObject "CognitoMemberDefinition" Core.$
      \x ->
        CognitoMemberDefinition'
          Core.<$> (x Core..: "UserPool")
          Core.<*> (x Core..: "UserGroup")
          Core.<*> (x Core..: "ClientId")
