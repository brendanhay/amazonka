{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatus
  ( ClientVpnAuthorizationRuleStatus (..)
  -- * Smart constructor
  , mkClientVpnAuthorizationRuleStatus
  -- * Lenses
  , cvarsCode
  , cvarsMessage
  ) where

import qualified Network.AWS.EC2.Types.ClientVpnAuthorizationRuleStatusCode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the state of an authorization rule.
--
-- /See:/ 'mkClientVpnAuthorizationRuleStatus' smart constructor.
data ClientVpnAuthorizationRuleStatus = ClientVpnAuthorizationRuleStatus'
  { code :: Core.Maybe Types.ClientVpnAuthorizationRuleStatusCode
    -- ^ The state of the authorization rule.
  , message :: Core.Maybe Core.Text
    -- ^ A message about the status of the authorization rule, if applicable.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClientVpnAuthorizationRuleStatus' value with any optional fields omitted.
mkClientVpnAuthorizationRuleStatus
    :: ClientVpnAuthorizationRuleStatus
mkClientVpnAuthorizationRuleStatus
  = ClientVpnAuthorizationRuleStatus'{code = Core.Nothing,
                                      message = Core.Nothing}

-- | The state of the authorization rule.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarsCode :: Lens.Lens' ClientVpnAuthorizationRuleStatus (Core.Maybe Types.ClientVpnAuthorizationRuleStatusCode)
cvarsCode = Lens.field @"code"
{-# INLINEABLE cvarsCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A message about the status of the authorization rule, if applicable.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvarsMessage :: Lens.Lens' ClientVpnAuthorizationRuleStatus (Core.Maybe Core.Text)
cvarsMessage = Lens.field @"message"
{-# INLINEABLE cvarsMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

instance Core.FromXML ClientVpnAuthorizationRuleStatus where
        parseXML x
          = ClientVpnAuthorizationRuleStatus' Core.<$>
              (x Core..@? "code") Core.<*> x Core..@? "message"
