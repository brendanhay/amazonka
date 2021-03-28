{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateAlias
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a fleet. In most situations, you can use an alias ID in place of a fleet ID. An alias provides a level of abstraction for a fleet that is useful when redirecting player traffic from one fleet to another, such as when updating your game build. 
--
-- Amazon GameLift supports two types of routing strategies for aliases: simple and terminal. A simple alias points to an active fleet. A terminal alias is used to display messaging or link to a URL instead of routing players to an active fleet. For example, you might use a terminal alias when a game version is no longer supported and you want to direct players to an upgrade site. 
-- To create a fleet alias, specify an alias name, routing strategy, and optional description. Each simple alias can point to only one fleet, but a fleet can have multiple aliases. If successful, a new alias record is returned, including an alias ID and an ARN. You can reassign an alias to another fleet by calling @UpdateAlias@ .
--
--     * 'CreateAlias' 
--
--
--     * 'ListAliases' 
--
--
--     * 'DescribeAlias' 
--
--
--     * 'UpdateAlias' 
--
--
--     * 'DeleteAlias' 
--
--
--     * 'ResolveAlias' 
--
--
module Network.AWS.GameLift.CreateAlias
    (
    -- * Creating a request
      CreateAlias (..)
    , mkCreateAlias
    -- ** Request lenses
    , caName
    , caRoutingStrategy
    , caDescription
    , caTags

    -- * Destructuring the response
    , CreateAliasResponse (..)
    , mkCreateAliasResponse
    -- ** Response lenses
    , carrsAlias
    , carrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateAlias' smart constructor.
data CreateAlias = CreateAlias'
  { name :: Types.Name
    -- ^ A descriptive label that is associated with an alias. Alias names do not need to be unique.
  , routingStrategy :: Types.RoutingStrategy
    -- ^ The routing configuration, including routing type and fleet target, for the alias. 
  , description :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A human-readable description of the alias.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAlias' value with any optional fields omitted.
mkCreateAlias
    :: Types.Name -- ^ 'name'
    -> Types.RoutingStrategy -- ^ 'routingStrategy'
    -> CreateAlias
mkCreateAlias name routingStrategy
  = CreateAlias'{name, routingStrategy, description = Core.Nothing,
                 tags = Core.Nothing}

-- | A descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateAlias Types.Name
caName = Lens.field @"name"
{-# INLINEABLE caName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The routing configuration, including routing type and fleet target, for the alias. 
--
-- /Note:/ Consider using 'routingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRoutingStrategy :: Lens.Lens' CreateAlias Types.RoutingStrategy
caRoutingStrategy = Lens.field @"routingStrategy"
{-# INLINEABLE caRoutingStrategy #-}
{-# DEPRECATED routingStrategy "Use generic-lens or generic-optics with 'routingStrategy' instead"  #-}

-- | A human-readable description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateAlias (Core.Maybe Types.NonZeroAndMaxString)
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of labels to assign to the new alias resource. Tags are developer-defined key-value pairs. Tagging AWS resources are useful for resource management, access management and cost allocation. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in the /AWS General Reference/ . Once the resource is created, you can use 'TagResource' , 'UntagResource' , and 'ListTagsForResource' to add, remove, and view tags. The maximum tag limit may be lower than stated. See the AWS General Reference for actual tagging limits.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTags :: Lens.Lens' CreateAlias (Core.Maybe [Types.Tag])
caTags = Lens.field @"tags"
{-# INLINEABLE caTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateAlias where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateAlias where
        toHeaders CreateAlias{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreateAlias") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateAlias where
        toJSON CreateAlias{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  Core.Just ("RoutingStrategy" Core..= routingStrategy),
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateAlias where
        type Rs CreateAlias = CreateAliasResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateAliasResponse' Core.<$>
                   (x Core..:? "Alias") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkCreateAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { alias :: Core.Maybe Types.Alias
    -- ^ The newly created alias resource.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateAliasResponse' value with any optional fields omitted.
mkCreateAliasResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateAliasResponse
mkCreateAliasResponse responseStatus
  = CreateAliasResponse'{alias = Core.Nothing, responseStatus}

-- | The newly created alias resource.
--
-- /Note:/ Consider using 'alias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAlias :: Lens.Lens' CreateAliasResponse (Core.Maybe Types.Alias)
carrsAlias = Lens.field @"alias"
{-# INLINEABLE carrsAlias #-}
{-# DEPRECATED alias "Use generic-lens or generic-optics with 'alias' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateAliasResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
