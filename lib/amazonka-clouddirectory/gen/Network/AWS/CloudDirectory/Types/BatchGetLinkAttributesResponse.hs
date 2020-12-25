{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetLinkAttributesResponse
  ( BatchGetLinkAttributesResponse (..),

    -- * Smart constructor
    mkBatchGetLinkAttributesResponse,

    -- * Lenses
    bglarAttributes,
  )
where

import qualified Network.AWS.CloudDirectory.Types.AttributeKeyAndValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'GetLinkAttributes' response operation.
--
-- /See:/ 'mkBatchGetLinkAttributesResponse' smart constructor.
newtype BatchGetLinkAttributesResponse = BatchGetLinkAttributesResponse'
  { -- | The attributes that are associated with the typed link.
    attributes :: Core.Maybe [Types.AttributeKeyAndValue]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'BatchGetLinkAttributesResponse' value with any optional fields omitted.
mkBatchGetLinkAttributesResponse ::
  BatchGetLinkAttributesResponse
mkBatchGetLinkAttributesResponse =
  BatchGetLinkAttributesResponse' {attributes = Core.Nothing}

-- | The attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bglarAttributes :: Lens.Lens' BatchGetLinkAttributesResponse (Core.Maybe [Types.AttributeKeyAndValue])
bglarAttributes = Lens.field @"attributes"
{-# DEPRECATED bglarAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON BatchGetLinkAttributesResponse where
  parseJSON =
    Core.withObject "BatchGetLinkAttributesResponse" Core.$
      \x ->
        BatchGetLinkAttributesResponse' Core.<$> (x Core..:? "Attributes")
