{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.STS.Types.FederatedUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.STS.Types.FederatedUser
  ( FederatedUser (..)
  -- * Smart constructor
  , mkFederatedUser
  -- * Lenses
  , fuFederatedUserId
  , fuArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.STS.Types.Arn as Types
import qualified Network.AWS.STS.Types.FederatedUserId as Types

-- | Identifiers for the federated user that is associated with the credentials.
--
-- /See:/ 'mkFederatedUser' smart constructor.
data FederatedUser = FederatedUser'
  { federatedUserId :: Types.FederatedUserId
    -- ^ The string that identifies the federated user associated with the credentials, similar to the unique ID of an IAM user.
  , arn :: Types.Arn
    -- ^ The ARN that specifies the federated user that is associated with the credentials. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FederatedUser' value with any optional fields omitted.
mkFederatedUser
    :: Types.FederatedUserId -- ^ 'federatedUserId'
    -> Types.Arn -- ^ 'arn'
    -> FederatedUser
mkFederatedUser federatedUserId arn
  = FederatedUser'{federatedUserId, arn}

-- | The string that identifies the federated user associated with the credentials, similar to the unique ID of an IAM user.
--
-- /Note:/ Consider using 'federatedUserId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuFederatedUserId :: Lens.Lens' FederatedUser Types.FederatedUserId
fuFederatedUserId = Lens.field @"federatedUserId"
{-# INLINEABLE fuFederatedUserId #-}
{-# DEPRECATED federatedUserId "Use generic-lens or generic-optics with 'federatedUserId' instead"  #-}

-- | The ARN that specifies the federated user that is associated with the credentials. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ . 
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fuArn :: Lens.Lens' FederatedUser Types.Arn
fuArn = Lens.field @"arn"
{-# INLINEABLE fuArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.FromXML FederatedUser where
        parseXML x
          = FederatedUser' Core.<$>
              (x Core..@ "FederatedUserId") Core.<*> x Core..@ "Arn"
