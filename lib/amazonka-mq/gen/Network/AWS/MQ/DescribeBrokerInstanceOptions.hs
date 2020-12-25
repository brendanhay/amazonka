{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.DescribeBrokerInstanceOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available broker instance options.
module Network.AWS.MQ.DescribeBrokerInstanceOptions
  ( -- * Creating a request
    DescribeBrokerInstanceOptions (..),
    mkDescribeBrokerInstanceOptions,

    -- ** Request lenses
    dbioEngineType,
    dbioHostInstanceType,
    dbioMaxResults,
    dbioNextToken,
    dbioStorageType,

    -- * Destructuring the response
    DescribeBrokerInstanceOptionsResponse (..),
    mkDescribeBrokerInstanceOptionsResponse,

    -- ** Response lenses
    dbiorrsBrokerInstanceOptions,
    dbiorrsMaxResults,
    dbiorrsNextToken,
    dbiorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MQ.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeBrokerInstanceOptions' smart constructor.
data DescribeBrokerInstanceOptions = DescribeBrokerInstanceOptions'
  { -- | Filter response by engine type.
    engineType :: Core.Maybe Core.Text,
    -- | Filter response by host instance type.
    hostInstanceType :: Core.Maybe Core.Text,
    -- | The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter response by storage type.
    storageType :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBrokerInstanceOptions' value with any optional fields omitted.
mkDescribeBrokerInstanceOptions ::
  DescribeBrokerInstanceOptions
mkDescribeBrokerInstanceOptions =
  DescribeBrokerInstanceOptions'
    { engineType = Core.Nothing,
      hostInstanceType = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      storageType = Core.Nothing
    }

-- | Filter response by engine type.
--
-- /Note:/ Consider using 'engineType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioEngineType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioEngineType = Lens.field @"engineType"
{-# DEPRECATED dbioEngineType "Use generic-lens or generic-optics with 'engineType' instead." #-}

-- | Filter response by host instance type.
--
-- /Note:/ Consider using 'hostInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioHostInstanceType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioHostInstanceType = Lens.field @"hostInstanceType"
{-# DEPRECATED dbioHostInstanceType "Use generic-lens or generic-optics with 'hostInstanceType' instead." #-}

-- | The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioMaxResults :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Natural)
dbioMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dbioMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioNextToken :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbioNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filter response by storage type.
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbioStorageType :: Lens.Lens' DescribeBrokerInstanceOptions (Core.Maybe Core.Text)
dbioStorageType = Lens.field @"storageType"
{-# DEPRECATED dbioStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

instance Core.AWSRequest DescribeBrokerInstanceOptions where
  type
    Rs DescribeBrokerInstanceOptions =
      DescribeBrokerInstanceOptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v1/broker-instance-options",
        Core._rqQuery =
          Core.toQueryValue "engineType" Core.<$> engineType
            Core.<> (Core.toQueryValue "hostInstanceType" Core.<$> hostInstanceType)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "storageType" Core.<$> storageType),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBrokerInstanceOptionsResponse'
            Core.<$> (x Core..:? "brokerInstanceOptions")
            Core.<*> (x Core..:? "maxResults")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeBrokerInstanceOptionsResponse' smart constructor.
data DescribeBrokerInstanceOptionsResponse = DescribeBrokerInstanceOptionsResponse'
  { -- | List of available broker instance options.
    brokerInstanceOptions :: Core.Maybe [Types.BrokerInstanceOption],
    -- | Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeBrokerInstanceOptionsResponse' value with any optional fields omitted.
mkDescribeBrokerInstanceOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeBrokerInstanceOptionsResponse
mkDescribeBrokerInstanceOptionsResponse responseStatus =
  DescribeBrokerInstanceOptionsResponse'
    { brokerInstanceOptions =
        Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | List of available broker instance options.
--
-- /Note:/ Consider using 'brokerInstanceOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsBrokerInstanceOptions :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe [Types.BrokerInstanceOption])
dbiorrsBrokerInstanceOptions = Lens.field @"brokerInstanceOptions"
{-# DEPRECATED dbiorrsBrokerInstanceOptions "Use generic-lens or generic-optics with 'brokerInstanceOptions' instead." #-}

-- | Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsMaxResults :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe Core.Natural)
dbiorrsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dbiorrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsNextToken :: Lens.Lens' DescribeBrokerInstanceOptionsResponse (Core.Maybe Core.Text)
dbiorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dbiorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbiorrsResponseStatus :: Lens.Lens' DescribeBrokerInstanceOptionsResponse Core.Int
dbiorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dbiorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
