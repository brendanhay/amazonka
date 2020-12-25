{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListPolicyVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of the specified policy and identifies the default version.
module Network.AWS.IoT.ListPolicyVersions
  ( -- * Creating a request
    ListPolicyVersions (..),
    mkListPolicyVersions,

    -- ** Request lenses
    lpvPolicyName,

    -- * Destructuring the response
    ListPolicyVersionsResponse (..),
    mkListPolicyVersionsResponse,

    -- ** Response lenses
    lpvrrsPolicyVersions,
    lpvrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the ListPolicyVersions operation.
--
-- /See:/ 'mkListPolicyVersions' smart constructor.
newtype ListPolicyVersions = ListPolicyVersions'
  { -- | The policy name.
    policyName :: Types.PolicyName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListPolicyVersions' value with any optional fields omitted.
mkListPolicyVersions ::
  -- | 'policyName'
  Types.PolicyName ->
  ListPolicyVersions
mkListPolicyVersions policyName = ListPolicyVersions' {policyName}

-- | The policy name.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvPolicyName :: Lens.Lens' ListPolicyVersions Types.PolicyName
lpvPolicyName = Lens.field @"policyName"
{-# DEPRECATED lpvPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Core.AWSRequest ListPolicyVersions where
  type Rs ListPolicyVersions = ListPolicyVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/policies/" Core.<> (Core.toText policyName)
                Core.<> ("/version")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPolicyVersionsResponse'
            Core.<$> (x Core..:? "policyVersions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the ListPolicyVersions operation.
--
-- /See:/ 'mkListPolicyVersionsResponse' smart constructor.
data ListPolicyVersionsResponse = ListPolicyVersionsResponse'
  { -- | The policy versions.
    policyVersions :: Core.Maybe [Types.PolicyVersion],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPolicyVersionsResponse' value with any optional fields omitted.
mkListPolicyVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPolicyVersionsResponse
mkListPolicyVersionsResponse responseStatus =
  ListPolicyVersionsResponse'
    { policyVersions = Core.Nothing,
      responseStatus
    }

-- | The policy versions.
--
-- /Note:/ Consider using 'policyVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsPolicyVersions :: Lens.Lens' ListPolicyVersionsResponse (Core.Maybe [Types.PolicyVersion])
lpvrrsPolicyVersions = Lens.field @"policyVersions"
{-# DEPRECATED lpvrrsPolicyVersions "Use generic-lens or generic-optics with 'policyVersions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpvrrsResponseStatus :: Lens.Lens' ListPolicyVersionsResponse Core.Int
lpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
