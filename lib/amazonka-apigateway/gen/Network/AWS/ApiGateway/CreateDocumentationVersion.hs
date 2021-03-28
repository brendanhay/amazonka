{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateDocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.ApiGateway.CreateDocumentationVersion
    (
    -- * Creating a request
      CreateDocumentationVersion (..)
    , mkCreateDocumentationVersion
    -- ** Request lenses
    , cdvRestApiId
    , cdvDocumentationVersion
    , cdvDescription
    , cdvStageName

     -- * Destructuring the response
    , Types.DocumentationVersion (..)
    , Types.mkDocumentationVersion
    -- ** Response lenses
    , Types.dvCreatedDate
    , Types.dvDescription
    , Types.dvVersion
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a new documentation version of a given API.
--
-- /See:/ 'mkCreateDocumentationVersion' smart constructor.
data CreateDocumentationVersion = CreateDocumentationVersion'
  { restApiId :: Core.Text
    -- ^ [Required] The string identifier of the associated 'RestApi' .
  , documentationVersion :: Core.Text
    -- ^ [Required] The version identifier of the new snapshot.
  , description :: Core.Maybe Core.Text
    -- ^ A description about the new documentation snapshot.
  , stageName :: Core.Maybe Core.Text
    -- ^ The stage name to be associated with the new documentation snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDocumentationVersion' value with any optional fields omitted.
mkCreateDocumentationVersion
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'documentationVersion'
    -> CreateDocumentationVersion
mkCreateDocumentationVersion restApiId documentationVersion
  = CreateDocumentationVersion'{restApiId, documentationVersion,
                                description = Core.Nothing, stageName = Core.Nothing}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvRestApiId :: Lens.Lens' CreateDocumentationVersion Core.Text
cdvRestApiId = Lens.field @"restApiId"
{-# INLINEABLE cdvRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The version identifier of the new snapshot.
--
-- /Note:/ Consider using 'documentationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvDocumentationVersion :: Lens.Lens' CreateDocumentationVersion Core.Text
cdvDocumentationVersion = Lens.field @"documentationVersion"
{-# INLINEABLE cdvDocumentationVersion #-}
{-# DEPRECATED documentationVersion "Use generic-lens or generic-optics with 'documentationVersion' instead"  #-}

-- | A description about the new documentation snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvDescription :: Lens.Lens' CreateDocumentationVersion (Core.Maybe Core.Text)
cdvDescription = Lens.field @"description"
{-# INLINEABLE cdvDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The stage name to be associated with the new documentation snapshot.
--
-- /Note:/ Consider using 'stageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdvStageName :: Lens.Lens' CreateDocumentationVersion (Core.Maybe Core.Text)
cdvStageName = Lens.field @"stageName"
{-# INLINEABLE cdvStageName #-}
{-# DEPRECATED stageName "Use generic-lens or generic-optics with 'stageName' instead"  #-}

instance Core.ToQuery CreateDocumentationVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateDocumentationVersion where
        toHeaders CreateDocumentationVersion{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateDocumentationVersion where
        toJSON CreateDocumentationVersion{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("documentationVersion" Core..= documentationVersion),
                  ("description" Core..=) Core.<$> description,
                  ("stageName" Core..=) Core.<$> stageName])

instance Core.AWSRequest CreateDocumentationVersion where
        type Rs CreateDocumentationVersion = Types.DocumentationVersion
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<>
                             "/documentation/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
