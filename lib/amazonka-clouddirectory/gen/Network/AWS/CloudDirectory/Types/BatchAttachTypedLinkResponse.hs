{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachTypedLinkResponse
  ( BatchAttachTypedLinkResponse (..),

    -- * Smart constructor
    mkBatchAttachTypedLinkResponse,

    -- * Lenses
    batlrTypedLinkSpecifier,
  )
where

import qualified Network.AWS.CloudDirectory.Types.TypedLinkSpecifier as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the output of a 'AttachTypedLink' response operation.
--
-- /See:/ 'mkBatchAttachTypedLinkResponse' smart constructor.
newtype BatchAttachTypedLinkResponse = BatchAttachTypedLinkResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifier :: Core.Maybe Types.TypedLinkSpecifier
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.NFData)

-- | Creates a 'BatchAttachTypedLinkResponse' value with any optional fields omitted.
mkBatchAttachTypedLinkResponse ::
  BatchAttachTypedLinkResponse
mkBatchAttachTypedLinkResponse =
  BatchAttachTypedLinkResponse' {typedLinkSpecifier = Core.Nothing}

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
batlrTypedLinkSpecifier :: Lens.Lens' BatchAttachTypedLinkResponse (Core.Maybe Types.TypedLinkSpecifier)
batlrTypedLinkSpecifier = Lens.field @"typedLinkSpecifier"
{-# DEPRECATED batlrTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Core.FromJSON BatchAttachTypedLinkResponse where
  parseJSON =
    Core.withObject "BatchAttachTypedLinkResponse" Core.$
      \x ->
        BatchAttachTypedLinkResponse'
          Core.<$> (x Core..:? "TypedLinkSpecifier")
