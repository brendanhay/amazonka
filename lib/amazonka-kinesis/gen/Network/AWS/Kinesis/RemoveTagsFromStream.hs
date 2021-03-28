{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.RemoveTagsFromStream
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from the specified Kinesis data stream. Removed tags are deleted and cannot be recovered after this operation successfully completes.
--
-- If you specify a tag that does not exist, it is ignored.
-- 'RemoveTagsFromStream' has a limit of five transactions per second per account.
module Network.AWS.Kinesis.RemoveTagsFromStream
    (
    -- * Creating a request
      RemoveTagsFromStream (..)
    , mkRemoveTagsFromStream
    -- ** Request lenses
    , rtfsStreamName
    , rtfsTagKeys

    -- * Destructuring the response
    , RemoveTagsFromStreamResponse (..)
    , mkRemoveTagsFromStreamResponse
    ) where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for @RemoveTagsFromStream@ .
--
-- /See:/ 'mkRemoveTagsFromStream' smart constructor.
data RemoveTagsFromStream = RemoveTagsFromStream'
  { streamName :: Types.StreamName
    -- ^ The name of the stream.
  , tagKeys :: Core.NonEmpty Types.TagKey
    -- ^ A list of tag keys. Each corresponding tag is removed from the stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromStream' value with any optional fields omitted.
mkRemoveTagsFromStream
    :: Types.StreamName -- ^ 'streamName'
    -> Core.NonEmpty Types.TagKey -- ^ 'tagKeys'
    -> RemoveTagsFromStream
mkRemoveTagsFromStream streamName tagKeys
  = RemoveTagsFromStream'{streamName, tagKeys}

-- | The name of the stream.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfsStreamName :: Lens.Lens' RemoveTagsFromStream Types.StreamName
rtfsStreamName = Lens.field @"streamName"
{-# INLINEABLE rtfsStreamName #-}
{-# DEPRECATED streamName "Use generic-lens or generic-optics with 'streamName' instead"  #-}

-- | A list of tag keys. Each corresponding tag is removed from the stream.
--
-- /Note:/ Consider using 'tagKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtfsTagKeys :: Lens.Lens' RemoveTagsFromStream (Core.NonEmpty Types.TagKey)
rtfsTagKeys = Lens.field @"tagKeys"
{-# INLINEABLE rtfsTagKeys #-}
{-# DEPRECATED tagKeys "Use generic-lens or generic-optics with 'tagKeys' instead"  #-}

instance Core.ToQuery RemoveTagsFromStream where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveTagsFromStream where
        toHeaders RemoveTagsFromStream{..}
          = Core.pure
              ("X-Amz-Target", "Kinesis_20131202.RemoveTagsFromStream")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveTagsFromStream where
        toJSON RemoveTagsFromStream{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StreamName" Core..= streamName),
                  Core.Just ("TagKeys" Core..= tagKeys)])

instance Core.AWSRequest RemoveTagsFromStream where
        type Rs RemoveTagsFromStream = RemoveTagsFromStreamResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RemoveTagsFromStreamResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveTagsFromStreamResponse' smart constructor.
data RemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveTagsFromStreamResponse' value with any optional fields omitted.
mkRemoveTagsFromStreamResponse
    :: RemoveTagsFromStreamResponse
mkRemoveTagsFromStreamResponse = RemoveTagsFromStreamResponse'
