{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetDefaultPolicyVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the specified version of the specified policy as the policy's default (operative) version. This action affects all certificates to which the policy is attached. To list the principals the policy is attached to, use the ListPrincipalPolicy API.
module Network.AWS.IoT.SetDefaultPolicyVersion
    (
    -- * Creating a request
      SetDefaultPolicyVersion (..)
    , mkSetDefaultPolicyVersion
    -- ** Request lenses
    , sdpvPolicyName
    , sdpvPolicyVersionId

    -- * Destructuring the response
    , SetDefaultPolicyVersionResponse (..)
    , mkSetDefaultPolicyVersionResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetDefaultPolicyVersion operation.
--
-- /See:/ 'mkSetDefaultPolicyVersion' smart constructor.
data SetDefaultPolicyVersion = SetDefaultPolicyVersion'
  { policyName :: Types.PolicyName
    -- ^ The policy name.
  , policyVersionId :: Types.PolicyVersionId
    -- ^ The policy version ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultPolicyVersion' value with any optional fields omitted.
mkSetDefaultPolicyVersion
    :: Types.PolicyName -- ^ 'policyName'
    -> Types.PolicyVersionId -- ^ 'policyVersionId'
    -> SetDefaultPolicyVersion
mkSetDefaultPolicyVersion policyName policyVersionId
  = SetDefaultPolicyVersion'{policyName, policyVersionId}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyName :: Lens.Lens' SetDefaultPolicyVersion Types.PolicyName
sdpvPolicyName = Lens.field @"policyName"
{-# INLINEABLE sdpvPolicyName #-}
{-# DEPRECATED policyName "Use generic-lens or generic-optics with 'policyName' instead"  #-}

-- | The policy version ID.
--
-- /Note:/ Consider using 'policyVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdpvPolicyVersionId :: Lens.Lens' SetDefaultPolicyVersion Types.PolicyVersionId
sdpvPolicyVersionId = Lens.field @"policyVersionId"
{-# INLINEABLE sdpvPolicyVersionId #-}
{-# DEPRECATED policyVersionId "Use generic-lens or generic-optics with 'policyVersionId' instead"  #-}

instance Core.ToQuery SetDefaultPolicyVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetDefaultPolicyVersion where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetDefaultPolicyVersion where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest SetDefaultPolicyVersion where
        type Rs SetDefaultPolicyVersion = SetDefaultPolicyVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/policies/" Core.<> Core.toText policyName Core.<> "/version/"
                             Core.<> Core.toText policyVersionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull SetDefaultPolicyVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetDefaultPolicyVersionResponse' smart constructor.
data SetDefaultPolicyVersionResponse = SetDefaultPolicyVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDefaultPolicyVersionResponse' value with any optional fields omitted.
mkSetDefaultPolicyVersionResponse
    :: SetDefaultPolicyVersionResponse
mkSetDefaultPolicyVersionResponse
  = SetDefaultPolicyVersionResponse'
