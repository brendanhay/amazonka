{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
  ( UserPoolClientDescription (..)
  -- * Smart constructor
  , mkUserPoolClientDescription
  -- * Lenses
  , upcdClientId
  , upcdClientName
  , upcdUserPoolId
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ClientId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientName as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description of the user pool client.
--
-- /See:/ 'mkUserPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
  { clientId :: Core.Maybe Types.ClientId
    -- ^ The ID of the client associated with the user pool.
  , clientName :: Core.Maybe Types.ClientName
    -- ^ The client name from the user pool client description.
  , userPoolId :: Core.Maybe Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to describe the user pool client.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserPoolClientDescription' value with any optional fields omitted.
mkUserPoolClientDescription
    :: UserPoolClientDescription
mkUserPoolClientDescription
  = UserPoolClientDescription'{clientId = Core.Nothing,
                               clientName = Core.Nothing, userPoolId = Core.Nothing}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcdClientId :: Lens.Lens' UserPoolClientDescription (Core.Maybe Types.ClientId)
upcdClientId = Lens.field @"clientId"
{-# INLINEABLE upcdClientId #-}
{-# DEPRECATED clientId "Use generic-lens or generic-optics with 'clientId' instead"  #-}

-- | The client name from the user pool client description.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcdClientName :: Lens.Lens' UserPoolClientDescription (Core.Maybe Types.ClientName)
upcdClientName = Lens.field @"clientName"
{-# INLINEABLE upcdClientName #-}
{-# DEPRECATED clientName "Use generic-lens or generic-optics with 'clientName' instead"  #-}

-- | The user pool ID for the user pool where you want to describe the user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upcdUserPoolId :: Lens.Lens' UserPoolClientDescription (Core.Maybe Types.UserPoolId)
upcdUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE upcdUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

instance Core.FromJSON UserPoolClientDescription where
        parseJSON
          = Core.withObject "UserPoolClientDescription" Core.$
              \ x ->
                UserPoolClientDescription' Core.<$>
                  (x Core..:? "ClientId") Core.<*> x Core..:? "ClientName" Core.<*>
                    x Core..:? "UserPoolId"
