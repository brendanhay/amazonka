{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CidrAuthorizationContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CidrAuthorizationContext
  ( CidrAuthorizationContext (..)
  -- * Smart constructor
  , mkCidrAuthorizationContext
  -- * Lenses
  , cacMessage
  , cacSignature
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides authorization for Amazon to bring a specific IP address range to a specific AWS account using bring your own IP addresses (BYOIP). For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html#prepare-for-byoip Prepare to Bring Your Address Range to Your AWS Account> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkCidrAuthorizationContext' smart constructor.
data CidrAuthorizationContext = CidrAuthorizationContext'
  { message :: Core.Text
    -- ^ The plain-text authorization message for the prefix and account.
  , signature :: Core.Text
    -- ^ The signed authorization message for the prefix and account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CidrAuthorizationContext' value with any optional fields omitted.
mkCidrAuthorizationContext
    :: Core.Text -- ^ 'message'
    -> Core.Text -- ^ 'signature'
    -> CidrAuthorizationContext
mkCidrAuthorizationContext message signature
  = CidrAuthorizationContext'{message, signature}

-- | The plain-text authorization message for the prefix and account.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacMessage :: Lens.Lens' CidrAuthorizationContext Core.Text
cacMessage = Lens.field @"message"
{-# INLINEABLE cacMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The signed authorization message for the prefix and account.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacSignature :: Lens.Lens' CidrAuthorizationContext Core.Text
cacSignature = Lens.field @"signature"
{-# INLINEABLE cacSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

instance Core.ToQuery CidrAuthorizationContext where
        toQuery CidrAuthorizationContext{..}
          = Core.toQueryPair "Message" message Core.<>
              Core.toQueryPair "Signature" signature
