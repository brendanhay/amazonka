{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeCreateAccountStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current status of an asynchronous request to create an account.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeCreateAccountStatus
  ( -- * Creating a request
    DescribeCreateAccountStatus (..),
    mkDescribeCreateAccountStatus,

    -- ** Request lenses
    dcasCreateAccountRequestId,

    -- * Destructuring the response
    DescribeCreateAccountStatusResponse (..),
    mkDescribeCreateAccountStatusResponse,

    -- ** Response lenses
    dcasrrsCreateAccountStatus,
    dcasrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Organizations.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCreateAccountStatus' smart constructor.
newtype DescribeCreateAccountStatus = DescribeCreateAccountStatus'
  { -- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
    createAccountRequestId :: Types.CreateAccountRequestId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCreateAccountStatus' value with any optional fields omitted.
mkDescribeCreateAccountStatus ::
  -- | 'createAccountRequestId'
  Types.CreateAccountRequestId ->
  DescribeCreateAccountStatus
mkDescribeCreateAccountStatus createAccountRequestId =
  DescribeCreateAccountStatus' {createAccountRequestId}

-- | Specifies the @Id@ value that uniquely identifies the @CreateAccount@ request. You can get the value from the @CreateAccountStatus.Id@ response in an earlier 'CreateAccount' request, or from the 'ListCreateAccountStatus' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a create account request ID string requires "car-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'createAccountRequestId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasCreateAccountRequestId :: Lens.Lens' DescribeCreateAccountStatus Types.CreateAccountRequestId
dcasCreateAccountRequestId = Lens.field @"createAccountRequestId"
{-# DEPRECATED dcasCreateAccountRequestId "Use generic-lens or generic-optics with 'createAccountRequestId' instead." #-}

instance Core.FromJSON DescribeCreateAccountStatus where
  toJSON DescribeCreateAccountStatus {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CreateAccountRequestId" Core..= createAccountRequestId)
          ]
      )

instance Core.AWSRequest DescribeCreateAccountStatus where
  type
    Rs DescribeCreateAccountStatus =
      DescribeCreateAccountStatusResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSOrganizationsV20161128.DescribeCreateAccountStatus"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCreateAccountStatusResponse'
            Core.<$> (x Core..:? "CreateAccountStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCreateAccountStatusResponse' smart constructor.
data DescribeCreateAccountStatusResponse = DescribeCreateAccountStatusResponse'
  { -- | A structure that contains the current status of an account creation request.
    createAccountStatus :: Core.Maybe Types.CreateAccountStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCreateAccountStatusResponse' value with any optional fields omitted.
mkDescribeCreateAccountStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCreateAccountStatusResponse
mkDescribeCreateAccountStatusResponse responseStatus =
  DescribeCreateAccountStatusResponse'
    { createAccountStatus =
        Core.Nothing,
      responseStatus
    }

-- | A structure that contains the current status of an account creation request.
--
-- /Note:/ Consider using 'createAccountStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrrsCreateAccountStatus :: Lens.Lens' DescribeCreateAccountStatusResponse (Core.Maybe Types.CreateAccountStatus)
dcasrrsCreateAccountStatus = Lens.field @"createAccountStatus"
{-# DEPRECATED dcasrrsCreateAccountStatus "Use generic-lens or generic-optics with 'createAccountStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcasrrsResponseStatus :: Lens.Lens' DescribeCreateAccountStatusResponse Core.Int
dcasrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcasrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
