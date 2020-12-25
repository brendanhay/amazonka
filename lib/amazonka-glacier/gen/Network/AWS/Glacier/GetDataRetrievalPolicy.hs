{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetDataRetrievalPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns the current data retrieval policy for the account and region specified in the GET request. For more information about data retrieval policies, see <https://docs.aws.amazon.com/amazonglacier/latest/dev/data-retrieval-policy.html Amazon Glacier Data Retrieval Policies> .
module Network.AWS.Glacier.GetDataRetrievalPolicy
  ( -- * Creating a request
    GetDataRetrievalPolicy (..),
    mkGetDataRetrievalPolicy,

    -- ** Request lenses
    gdrpAccountId,

    -- * Destructuring the response
    GetDataRetrievalPolicyResponse (..),
    mkGetDataRetrievalPolicyResponse,

    -- ** Response lenses
    gdrprrsPolicy,
    gdrprrsResponseStatus,
  )
where

import qualified Network.AWS.Glacier.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for GetDataRetrievalPolicy.
--
-- /See:/ 'mkGetDataRetrievalPolicy' smart constructor.
newtype GetDataRetrievalPolicy = GetDataRetrievalPolicy'
  { -- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
    accountId :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataRetrievalPolicy' value with any optional fields omitted.
mkGetDataRetrievalPolicy ::
  -- | 'accountId'
  Types.String ->
  GetDataRetrievalPolicy
mkGetDataRetrievalPolicy accountId =
  GetDataRetrievalPolicy' {accountId}

-- | The @AccountId@ value is the AWS account ID. This value must match the AWS account ID associated with the credentials used to sign the request. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you specify your account ID, do not include any hyphens ('-') in the ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrpAccountId :: Lens.Lens' GetDataRetrievalPolicy Types.String
gdrpAccountId = Lens.field @"accountId"
{-# DEPRECATED gdrpAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Core.AWSRequest GetDataRetrievalPolicy where
  type Rs GetDataRetrievalPolicy = GetDataRetrievalPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/" Core.<> (Core.toText accountId)
                Core.<> ("/policies/data-retrieval")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataRetrievalPolicyResponse'
            Core.<$> (x Core..:? "Policy") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the Amazon S3 Glacier response to the @GetDataRetrievalPolicy@ request.
--
-- /See:/ 'mkGetDataRetrievalPolicyResponse' smart constructor.
data GetDataRetrievalPolicyResponse = GetDataRetrievalPolicyResponse'
  { -- | Contains the returned data retrieval policy in JSON format.
    policy :: Core.Maybe Types.DataRetrievalPolicy,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDataRetrievalPolicyResponse' value with any optional fields omitted.
mkGetDataRetrievalPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDataRetrievalPolicyResponse
mkGetDataRetrievalPolicyResponse responseStatus =
  GetDataRetrievalPolicyResponse'
    { policy = Core.Nothing,
      responseStatus
    }

-- | Contains the returned data retrieval policy in JSON format.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsPolicy :: Lens.Lens' GetDataRetrievalPolicyResponse (Core.Maybe Types.DataRetrievalPolicy)
gdrprrsPolicy = Lens.field @"policy"
{-# DEPRECATED gdrprrsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrprrsResponseStatus :: Lens.Lens' GetDataRetrievalPolicyResponse Core.Int
gdrprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
