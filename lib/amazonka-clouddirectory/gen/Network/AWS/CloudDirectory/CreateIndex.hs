{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.html Indexing and search> for more information.
module Network.AWS.CloudDirectory.CreateIndex
    (
    -- * Creating a request
      CreateIndex (..)
    , mkCreateIndex
    -- ** Request lenses
    , ciDirectoryArn
    , ciOrderedIndexedAttributeList
    , ciIsUnique
    , ciLinkName
    , ciParentReference

    -- * Destructuring the response
    , CreateIndexResponse (..)
    , mkCreateIndexResponse
    -- ** Response lenses
    , cirrsObjectIdentifier
    , cirrsResponseStatus
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateIndex' smart constructor.
data CreateIndex = CreateIndex'
  { directoryArn :: Types.Arn
    -- ^ The ARN of the directory where the index should be created.
  , orderedIndexedAttributeList :: [Types.AttributeKey]
    -- ^ Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
  , isUnique :: Core.Bool
    -- ^ Indicates whether the attribute that is being indexed has unique values or not.
  , linkName :: Core.Maybe Types.LinkName
    -- ^ The name of the link between the parent object and the index object.
  , parentReference :: Core.Maybe Types.ObjectReference
    -- ^ A reference to the parent object that contains the index object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIndex' value with any optional fields omitted.
mkCreateIndex
    :: Types.Arn -- ^ 'directoryArn'
    -> Core.Bool -- ^ 'isUnique'
    -> CreateIndex
mkCreateIndex directoryArn isUnique
  = CreateIndex'{directoryArn,
                 orderedIndexedAttributeList = Core.mempty, isUnique,
                 linkName = Core.Nothing, parentReference = Core.Nothing}

-- | The ARN of the directory where the index should be created.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDirectoryArn :: Lens.Lens' CreateIndex Types.Arn
ciDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE ciDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
--
-- /Note:/ Consider using 'orderedIndexedAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOrderedIndexedAttributeList :: Lens.Lens' CreateIndex [Types.AttributeKey]
ciOrderedIndexedAttributeList = Lens.field @"orderedIndexedAttributeList"
{-# INLINEABLE ciOrderedIndexedAttributeList #-}
{-# DEPRECATED orderedIndexedAttributeList "Use generic-lens or generic-optics with 'orderedIndexedAttributeList' instead"  #-}

-- | Indicates whether the attribute that is being indexed has unique values or not.
--
-- /Note:/ Consider using 'isUnique' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciIsUnique :: Lens.Lens' CreateIndex Core.Bool
ciIsUnique = Lens.field @"isUnique"
{-# INLINEABLE ciIsUnique #-}
{-# DEPRECATED isUnique "Use generic-lens or generic-optics with 'isUnique' instead"  #-}

-- | The name of the link between the parent object and the index object.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLinkName :: Lens.Lens' CreateIndex (Core.Maybe Types.LinkName)
ciLinkName = Lens.field @"linkName"
{-# INLINEABLE ciLinkName #-}
{-# DEPRECATED linkName "Use generic-lens or generic-optics with 'linkName' instead"  #-}

-- | A reference to the parent object that contains the index object.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciParentReference :: Lens.Lens' CreateIndex (Core.Maybe Types.ObjectReference)
ciParentReference = Lens.field @"parentReference"
{-# INLINEABLE ciParentReference #-}
{-# DEPRECATED parentReference "Use generic-lens or generic-optics with 'parentReference' instead"  #-}

instance Core.ToQuery CreateIndex where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateIndex where
        toHeaders CreateIndex{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON CreateIndex where
        toJSON CreateIndex{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("OrderedIndexedAttributeList" Core..=
                       orderedIndexedAttributeList),
                  Core.Just ("IsUnique" Core..= isUnique),
                  ("LinkName" Core..=) Core.<$> linkName,
                  ("ParentReference" Core..=) Core.<$> parentReference])

instance Core.AWSRequest CreateIndex where
        type Rs CreateIndex = CreateIndexResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/index",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateIndexResponse' Core.<$>
                   (x Core..:? "ObjectIdentifier") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateIndexResponse' smart constructor.
data CreateIndexResponse = CreateIndexResponse'
  { objectIdentifier :: Core.Maybe Types.ObjectIdentifier
    -- ^ The @ObjectIdentifier@ of the index created by this operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIndexResponse' value with any optional fields omitted.
mkCreateIndexResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateIndexResponse
mkCreateIndexResponse responseStatus
  = CreateIndexResponse'{objectIdentifier = Core.Nothing,
                         responseStatus}

-- | The @ObjectIdentifier@ of the index created by this operation.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsObjectIdentifier :: Lens.Lens' CreateIndexResponse (Core.Maybe Types.ObjectIdentifier)
cirrsObjectIdentifier = Lens.field @"objectIdentifier"
{-# INLINEABLE cirrsObjectIdentifier #-}
{-# DEPRECATED objectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirrsResponseStatus :: Lens.Lens' CreateIndexResponse Core.Int
cirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
