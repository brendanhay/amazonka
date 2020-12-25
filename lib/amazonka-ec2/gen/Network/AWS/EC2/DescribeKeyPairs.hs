{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeKeyPairs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified key pairs or all of your key pairs.
--
-- For more information about key pairs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html Key Pairs> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeKeyPairs
  ( -- * Creating a request
    DescribeKeyPairs (..),
    mkDescribeKeyPairs,

    -- ** Request lenses
    dkpsDryRun,
    dkpsFilters,
    dkpsKeyNames,
    dkpsKeyPairIds,

    -- * Destructuring the response
    DescribeKeyPairsResponse (..),
    mkDescribeKeyPairsResponse,

    -- ** Response lenses
    dkprrsKeyPairs,
    dkprrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeKeyPairs' smart constructor.
data DescribeKeyPairs = DescribeKeyPairs'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @key-pair-id@ - The ID of the key pair.
    --
    --
    --     * @fingerprint@ - The fingerprint of the key pair.
    --
    --
    --     * @key-name@ - The name of the key pair.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    filters :: Core.Maybe [Types.Filter],
    -- | The key pair names.
    --
    -- Default: Describes all your key pairs.
    keyNames :: Core.Maybe [Types.KeyPairName],
    -- | The IDs of the key pairs.
    keyPairIds :: Core.Maybe [Types.KeyPairId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeKeyPairs' value with any optional fields omitted.
mkDescribeKeyPairs ::
  DescribeKeyPairs
mkDescribeKeyPairs =
  DescribeKeyPairs'
    { dryRun = Core.Nothing,
      filters = Core.Nothing,
      keyNames = Core.Nothing,
      keyPairIds = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpsDryRun :: Lens.Lens' DescribeKeyPairs (Core.Maybe Core.Bool)
dkpsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dkpsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @key-pair-id@ - The ID of the key pair.
--
--
--     * @fingerprint@ - The fingerprint of the key pair.
--
--
--     * @key-name@ - The name of the key pair.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpsFilters :: Lens.Lens' DescribeKeyPairs (Core.Maybe [Types.Filter])
dkpsFilters = Lens.field @"filters"
{-# DEPRECATED dkpsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The key pair names.
--
-- Default: Describes all your key pairs.
--
-- /Note:/ Consider using 'keyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpsKeyNames :: Lens.Lens' DescribeKeyPairs (Core.Maybe [Types.KeyPairName])
dkpsKeyNames = Lens.field @"keyNames"
{-# DEPRECATED dkpsKeyNames "Use generic-lens or generic-optics with 'keyNames' instead." #-}

-- | The IDs of the key pairs.
--
-- /Note:/ Consider using 'keyPairIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkpsKeyPairIds :: Lens.Lens' DescribeKeyPairs (Core.Maybe [Types.KeyPairId])
dkpsKeyPairIds = Lens.field @"keyPairIds"
{-# DEPRECATED dkpsKeyPairIds "Use generic-lens or generic-optics with 'keyPairIds' instead." #-}

instance Core.AWSRequest DescribeKeyPairs where
  type Rs DescribeKeyPairs = DescribeKeyPairsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DescribeKeyPairs")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "KeyName" Core.<$> keyNames)
                Core.<> (Core.toQueryList "KeyPairId" Core.<$> keyPairIds)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeKeyPairsResponse'
            Core.<$> (x Core..@? "keySet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeKeyPairsResponse' smart constructor.
data DescribeKeyPairsResponse = DescribeKeyPairsResponse'
  { -- | Information about the key pairs.
    keyPairs :: Core.Maybe [Types.KeyPairInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeKeyPairsResponse' value with any optional fields omitted.
mkDescribeKeyPairsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeKeyPairsResponse
mkDescribeKeyPairsResponse responseStatus =
  DescribeKeyPairsResponse'
    { keyPairs = Core.Nothing,
      responseStatus
    }

-- | Information about the key pairs.
--
-- /Note:/ Consider using 'keyPairs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsKeyPairs :: Lens.Lens' DescribeKeyPairsResponse (Core.Maybe [Types.KeyPairInfo])
dkprrsKeyPairs = Lens.field @"keyPairs"
{-# DEPRECATED dkprrsKeyPairs "Use generic-lens or generic-optics with 'keyPairs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkprrsResponseStatus :: Lens.Lens' DescribeKeyPairsResponse Core.Int
dkprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dkprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
