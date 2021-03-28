{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.DetachTypedLink
    (
    -- * Creating a request
      DetachTypedLink (..)
    , mkDetachTypedLink
    -- ** Request lenses
    , dtlDirectoryArn
    , dtlTypedLinkSpecifier

    -- * Destructuring the response
    , DetachTypedLinkResponse (..)
    , mkDetachTypedLinkResponse
    ) where

import qualified Network.AWS.CloudDirectory.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDetachTypedLink' smart constructor.
data DetachTypedLink = DetachTypedLink'
  { directoryArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the directory where you want to detach the typed link.
  , typedLinkSpecifier :: Types.TypedLinkSpecifier
    -- ^ Used to accept a typed link specifier as input.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DetachTypedLink' value with any optional fields omitted.
mkDetachTypedLink
    :: Types.Arn -- ^ 'directoryArn'
    -> Types.TypedLinkSpecifier -- ^ 'typedLinkSpecifier'
    -> DetachTypedLink
mkDetachTypedLink directoryArn typedLinkSpecifier
  = DetachTypedLink'{directoryArn, typedLinkSpecifier}

-- | The Amazon Resource Name (ARN) of the directory where you want to detach the typed link.
--
-- /Note:/ Consider using 'directoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlDirectoryArn :: Lens.Lens' DetachTypedLink Types.Arn
dtlDirectoryArn = Lens.field @"directoryArn"
{-# INLINEABLE dtlDirectoryArn #-}
{-# DEPRECATED directoryArn "Use generic-lens or generic-optics with 'directoryArn' instead"  #-}

-- | Used to accept a typed link specifier as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlTypedLinkSpecifier :: Lens.Lens' DetachTypedLink Types.TypedLinkSpecifier
dtlTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# INLINEABLE dtlTypedLinkSpecifier #-}
{-# DEPRECATED typedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead"  #-}

instance Core.ToQuery DetachTypedLink where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DetachTypedLink where
        toHeaders DetachTypedLink{..}
          = Core.toHeaders "x-amz-data-partition" directoryArn

instance Core.FromJSON DetachTypedLink where
        toJSON DetachTypedLink{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TypedLinkSpecifier" Core..= typedLinkSpecifier)])

instance Core.AWSRequest DetachTypedLink where
        type Rs DetachTypedLink = DetachTypedLinkResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/amazonclouddirectory/2017-01-11/typedlink/detach",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DetachTypedLinkResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDetachTypedLinkResponse' smart constructor.
data DetachTypedLinkResponse = DetachTypedLinkResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DetachTypedLinkResponse' value with any optional fields omitted.
mkDetachTypedLinkResponse
    :: DetachTypedLinkResponse
mkDetachTypedLinkResponse = DetachTypedLinkResponse'
