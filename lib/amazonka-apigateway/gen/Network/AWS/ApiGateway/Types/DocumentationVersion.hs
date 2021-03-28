{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.DocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.DocumentationVersion
  ( DocumentationVersion (..)
  -- * Smart constructor
  , mkDocumentationVersion
  -- * Lenses
  , dvCreatedDate
  , dvDescription
  , dvVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A snapshot of the documentation of an API.
--
-- Publishing API documentation involves creating a documentation version associated with an API stage and exporting the versioned documentation to an external (e.g., OpenAPI) file.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersions' 
--
-- /See:/ 'mkDocumentationVersion' smart constructor.
data DocumentationVersion = DocumentationVersion'
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the API documentation snapshot is created.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the API documentation snapshot.
  , version :: Core.Maybe Core.Text
    -- ^ The version identifier of the API documentation snapshot.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DocumentationVersion' value with any optional fields omitted.
mkDocumentationVersion
    :: DocumentationVersion
mkDocumentationVersion
  = DocumentationVersion'{createdDate = Core.Nothing,
                          description = Core.Nothing, version = Core.Nothing}

-- | The date when the API documentation snapshot is created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvCreatedDate :: Lens.Lens' DocumentationVersion (Core.Maybe Core.NominalDiffTime)
dvCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE dvCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The description of the API documentation snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDescription :: Lens.Lens' DocumentationVersion (Core.Maybe Core.Text)
dvDescription = Lens.field @"description"
{-# INLINEABLE dvDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The version identifier of the API documentation snapshot.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVersion :: Lens.Lens' DocumentationVersion (Core.Maybe Core.Text)
dvVersion = Lens.field @"version"
{-# INLINEABLE dvVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON DocumentationVersion where
        parseJSON
          = Core.withObject "DocumentationVersion" Core.$
              \ x ->
                DocumentationVersion' Core.<$>
                  (x Core..:? "createdDate") Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "version"
