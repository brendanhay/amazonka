{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudDirectory.Types.BatchListObjectAttributesResponse
  ( BatchListObjectAttributesResponse (..)
  -- * Smart constructor
  , mkBatchListObjectAttributesResponse
  -- * Lenses
  , bloarAttributes
  , bloarNextToken
  ) where

import qualified Network.AWS.CloudDirectory.Types.AttributeKeyAndValue as Types
import qualified Network.AWS.CloudDirectory.Types.NextToken as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'ListObjectAttributes' response operation.
--
-- /See:/ 'mkBatchListObjectAttributesResponse' smart constructor.
data BatchListObjectAttributesResponse = BatchListObjectAttributesResponse'
  { attributes :: Core.Maybe [Types.AttributeKeyAndValue]
    -- ^ The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchListObjectAttributesResponse' value with any optional fields omitted.
mkBatchListObjectAttributesResponse
    :: BatchListObjectAttributesResponse
mkBatchListObjectAttributesResponse
  = BatchListObjectAttributesResponse'{attributes = Core.Nothing,
                                       nextToken = Core.Nothing}

-- | The attributes map that is associated with the object. @AttributeArn@ is the key; attribute value is the value.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloarAttributes :: Lens.Lens' BatchListObjectAttributesResponse (Core.Maybe [Types.AttributeKeyAndValue])
bloarAttributes = Lens.field @"attributes"
{-# INLINEABLE bloarAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | The pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bloarNextToken :: Lens.Lens' BatchListObjectAttributesResponse (Core.Maybe Types.NextToken)
bloarNextToken = Lens.field @"nextToken"
{-# INLINEABLE bloarNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.FromJSON BatchListObjectAttributesResponse where
        parseJSON
          = Core.withObject "BatchListObjectAttributesResponse" Core.$
              \ x ->
                BatchListObjectAttributesResponse' Core.<$>
                  (x Core..:? "Attributes") Core.<*> x Core..:? "NextToken"
