{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a cluster.
--
-- A resource can have up to 50 tags. If you try to create more than 50 tags for a resource, you will receive an error and the attempt will fail.
-- If you specify a key that already exists for the resource, the value for that key will be updated with the new value.
module Network.AWS.Redshift.CreateTags
    (
    -- * Creating a request
      CreateTags (..)
    , mkCreateTags
    -- ** Request lenses
    , ctResourceName
    , ctTags

    -- * Destructuring the response
    , CreateTagsResponse (..)
    , mkCreateTagsResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the output from the @CreateTags@ action. 
--
-- /See:/ 'mkCreateTags' smart constructor.
data CreateTags = CreateTags'
  { resourceName :: Core.Text
    -- ^ The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ . 
  , tags :: [Types.Tag]
    -- ^ One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTags' value with any optional fields omitted.
mkCreateTags
    :: Core.Text -- ^ 'resourceName'
    -> CreateTags
mkCreateTags resourceName
  = CreateTags'{resourceName, tags = Core.mempty}

-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-2:123456789:cluster:t1@ . 
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctResourceName :: Lens.Lens' CreateTags Core.Text
ctResourceName = Lens.field @"resourceName"
{-# INLINEABLE ctResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ . 
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctTags :: Lens.Lens' CreateTags [Types.Tag]
ctTags = Lens.field @"tags"
{-# INLINEABLE ctTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateTags where
        toQuery CreateTags{..}
          = Core.toQueryPair "Action" ("CreateTags" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "ResourceName" resourceName
              Core.<> Core.toQueryPair "Tags" (Core.toQueryList "Tag" tags)

instance Core.ToHeaders CreateTags where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull CreateTagsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateTagsResponse' smart constructor.
data CreateTagsResponse = CreateTagsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateTagsResponse' value with any optional fields omitted.
mkCreateTagsResponse
    :: CreateTagsResponse
mkCreateTagsResponse = CreateTagsResponse'
