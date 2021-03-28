{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.CreateWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workgroup with the specified name.
module Network.AWS.Athena.CreateWorkGroup
    (
    -- * Creating a request
      CreateWorkGroup (..)
    , mkCreateWorkGroup
    -- ** Request lenses
    , cwgName
    , cwgConfiguration
    , cwgDescription
    , cwgTags

    -- * Destructuring the response
    , CreateWorkGroupResponse (..)
    , mkCreateWorkGroupResponse
    -- ** Response lenses
    , cwgrrsResponseStatus
    ) where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateWorkGroup' smart constructor.
data CreateWorkGroup = CreateWorkGroup'
  { name :: Types.Name
    -- ^ The workgroup name.
  , configuration :: Core.Maybe Types.WorkGroupConfiguration
    -- ^ The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for encrypting query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, the limit for the amount of bytes scanned (cutoff) per query, if it is specified, and whether workgroup's settings (specified with EnforceWorkGroupConfiguration) in the WorkGroupConfiguration override client-side settings. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
  , description :: Core.Maybe Types.Description
    -- ^ The workgroup description.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of comma separated tags to add to the workgroup that is created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkGroup' value with any optional fields omitted.
mkCreateWorkGroup
    :: Types.Name -- ^ 'name'
    -> CreateWorkGroup
mkCreateWorkGroup name
  = CreateWorkGroup'{name, configuration = Core.Nothing,
                     description = Core.Nothing, tags = Core.Nothing}

-- | The workgroup name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgName :: Lens.Lens' CreateWorkGroup Types.Name
cwgName = Lens.field @"name"
{-# INLINEABLE cwgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The configuration for the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for encrypting query results, whether the Amazon CloudWatch Metrics are enabled for the workgroup, the limit for the amount of bytes scanned (cutoff) per query, if it is specified, and whether workgroup's settings (specified with EnforceWorkGroupConfiguration) in the WorkGroupConfiguration override client-side settings. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgConfiguration :: Lens.Lens' CreateWorkGroup (Core.Maybe Types.WorkGroupConfiguration)
cwgConfiguration = Lens.field @"configuration"
{-# INLINEABLE cwgConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgDescription :: Lens.Lens' CreateWorkGroup (Core.Maybe Types.Description)
cwgDescription = Lens.field @"description"
{-# INLINEABLE cwgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of comma separated tags to add to the workgroup that is created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgTags :: Lens.Lens' CreateWorkGroup (Core.Maybe [Types.Tag])
cwgTags = Lens.field @"tags"
{-# INLINEABLE cwgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateWorkGroup where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateWorkGroup where
        toHeaders CreateWorkGroup{..}
          = Core.pure ("X-Amz-Target", "AmazonAthena.CreateWorkGroup")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateWorkGroup where
        toJSON CreateWorkGroup{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Name" Core..= name),
                  ("Configuration" Core..=) Core.<$> configuration,
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest CreateWorkGroup where
        type Rs CreateWorkGroup = CreateWorkGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateWorkGroupResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateWorkGroupResponse' smart constructor.
newtype CreateWorkGroupResponse = CreateWorkGroupResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateWorkGroupResponse' value with any optional fields omitted.
mkCreateWorkGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateWorkGroupResponse
mkCreateWorkGroupResponse responseStatus
  = CreateWorkGroupResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwgrrsResponseStatus :: Lens.Lens' CreateWorkGroupResponse Core.Int
cwgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cwgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
