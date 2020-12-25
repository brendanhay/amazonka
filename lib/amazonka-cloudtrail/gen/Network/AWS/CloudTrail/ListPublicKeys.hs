{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.ListPublicKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all public keys whose private keys were used to sign the digest files within the specified time range. The public key is needed to validate digest files that were signed with its corresponding private key.
--
-- This operation returns paginated results.
module Network.AWS.CloudTrail.ListPublicKeys
  ( -- * Creating a request
    ListPublicKeys (..),
    mkListPublicKeys,

    -- ** Request lenses
    lpkEndTime,
    lpkNextToken,
    lpkStartTime,

    -- * Destructuring the response
    ListPublicKeysResponse (..),
    mkListPublicKeysResponse,

    -- ** Response lenses
    lpkrrsNextToken,
    lpkrrsPublicKeyList,
    lpkrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudTrail.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Requests the public keys for a specified time range.
--
-- /See:/ 'mkListPublicKeys' smart constructor.
data ListPublicKeys = ListPublicKeys'
  { -- | Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Reserved for future use.
    nextToken :: Core.Maybe Types.String,
    -- | Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPublicKeys' value with any optional fields omitted.
mkListPublicKeys ::
  ListPublicKeys
mkListPublicKeys =
  ListPublicKeys'
    { endTime = Core.Nothing,
      nextToken = Core.Nothing,
      startTime = Core.Nothing
    }

-- | Optionally specifies, in UTC, the end of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkEndTime :: Lens.Lens' ListPublicKeys (Core.Maybe Core.NominalDiffTime)
lpkEndTime = Lens.field @"endTime"
{-# DEPRECATED lpkEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkNextToken :: Lens.Lens' ListPublicKeys (Core.Maybe Types.String)
lpkNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpkNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Optionally specifies, in UTC, the start of the time range to look up public keys for CloudTrail digest files. If not specified, the current time is used, and the current public key is returned.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkStartTime :: Lens.Lens' ListPublicKeys (Core.Maybe Core.NominalDiffTime)
lpkStartTime = Lens.field @"startTime"
{-# DEPRECATED lpkStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON ListPublicKeys where
  toJSON ListPublicKeys {..} =
    Core.object
      ( Core.catMaybes
          [ ("EndTime" Core..=) Core.<$> endTime,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("StartTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest ListPublicKeys where
  type Rs ListPublicKeys = ListPublicKeysResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListPublicKeys"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPublicKeysResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "PublicKeyList")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPublicKeys where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"publicKeyList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Returns the objects or data listed below if successful. Otherwise, returns an error.
--
-- /See:/ 'mkListPublicKeysResponse' smart constructor.
data ListPublicKeysResponse = ListPublicKeysResponse'
  { -- | Reserved for future use.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Contains an array of PublicKey objects.
    publicKeyList :: Core.Maybe [Types.PublicKey],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPublicKeysResponse' value with any optional fields omitted.
mkListPublicKeysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPublicKeysResponse
mkListPublicKeysResponse responseStatus =
  ListPublicKeysResponse'
    { nextToken = Core.Nothing,
      publicKeyList = Core.Nothing,
      responseStatus
    }

-- | Reserved for future use.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsNextToken :: Lens.Lens' ListPublicKeysResponse (Core.Maybe Types.NextToken)
lpkrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpkrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Contains an array of PublicKey objects.
--
-- /Note:/ Consider using 'publicKeyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsPublicKeyList :: Lens.Lens' ListPublicKeysResponse (Core.Maybe [Types.PublicKey])
lpkrrsPublicKeyList = Lens.field @"publicKeyList"
{-# DEPRECATED lpkrrsPublicKeyList "Use generic-lens or generic-optics with 'publicKeyList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpkrrsResponseStatus :: Lens.Lens' ListPublicKeysResponse Core.Int
lpkrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lpkrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
