{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.SetDataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation sets and then enacts a data retrieval policy in the region specified in the PUT request. You can set one policy per region for an AWS account. The policy is enacted within a few minutes of a successful PUT operation.
--
-- The set policy operation does not affect retrieval jobs that were in progress before the policy was enacted. For more information about data retrieval policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies> . 
module Network.AWS.Glacier.SetDataRetrievalPolicy
    (
    -- * Creating a request
      SetDataRetrievalPolicy (..)
    , mkSetDataRetrievalPolicy
    -- ** Request lenses
    , sdrpAccountId
    , sdrpPolicy

    -- * Destructuring the response
    , SetDataRetrievalPolicyResponse (..)
    , mkSetDataRetrievalPolicyResponse
    ) where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | SetDataRetrievalPolicy input.
--
-- /See:/ 'mkSetDataRetrievalPolicy' smart constructor.
data SetDataRetrievalPolicy = SetDataRetrievalPolicy'
  { accountId :: Core.Text
    -- ^ The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
  , policy :: Core.Maybe Types.DataRetrievalPolicy
    -- ^ The data retrieval policy in JSON format.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDataRetrievalPolicy' value with any optional fields omitted.
mkSetDataRetrievalPolicy
    :: Core.Text -- ^ 'accountId'
    -> SetDataRetrievalPolicy
mkSetDataRetrievalPolicy accountId
  = SetDataRetrievalPolicy'{accountId, policy = Core.Nothing}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrpAccountId :: Lens.Lens' SetDataRetrievalPolicy Core.Text
sdrpAccountId = Lens.field @"accountId"
{-# INLINEABLE sdrpAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The data retrieval policy in JSON format.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrpPolicy :: Lens.Lens' SetDataRetrievalPolicy (Core.Maybe Types.DataRetrievalPolicy)
sdrpPolicy = Lens.field @"policy"
{-# INLINEABLE sdrpPolicy #-}
{-# DEPRECATED policy "Use generic-lens or generic-optics with 'policy' instead"  #-}

instance Core.ToQuery SetDataRetrievalPolicy where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetDataRetrievalPolicy where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetDataRetrievalPolicy where
        toJSON SetDataRetrievalPolicy{..}
          = Core.object (Core.catMaybes [("Policy" Core..=) Core.<$> policy])

instance Core.AWSRequest SetDataRetrievalPolicy where
        type Rs SetDataRetrievalPolicy = SetDataRetrievalPolicyResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText accountId Core.<>
                             "/policies/data-retrieval",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull SetDataRetrievalPolicyResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetDataRetrievalPolicyResponse' smart constructor.
data SetDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDataRetrievalPolicyResponse' value with any optional fields omitted.
mkSetDataRetrievalPolicyResponse
    :: SetDataRetrievalPolicyResponse
mkSetDataRetrievalPolicyResponse = SetDataRetrievalPolicyResponse'
