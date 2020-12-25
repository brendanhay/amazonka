{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureContentTypeHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CaptureContentTypeHeader
  ( CaptureContentTypeHeader (..),

    -- * Smart constructor
    mkCaptureContentTypeHeader,

    -- * Lenses
    ccthCsvContentTypes,
    ccthJsonContentTypes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CsvContentType as Types
import qualified Network.AWS.SageMaker.Types.JsonContentType as Types

-- |
--
-- /See:/ 'mkCaptureContentTypeHeader' smart constructor.
data CaptureContentTypeHeader = CaptureContentTypeHeader'
  { -- |
    csvContentTypes :: Core.Maybe (Core.NonEmpty Types.CsvContentType),
    -- |
    jsonContentTypes :: Core.Maybe (Core.NonEmpty Types.JsonContentType)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CaptureContentTypeHeader' value with any optional fields omitted.
mkCaptureContentTypeHeader ::
  CaptureContentTypeHeader
mkCaptureContentTypeHeader =
  CaptureContentTypeHeader'
    { csvContentTypes = Core.Nothing,
      jsonContentTypes = Core.Nothing
    }

-- |
--
-- /Note:/ Consider using 'csvContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccthCsvContentTypes :: Lens.Lens' CaptureContentTypeHeader (Core.Maybe (Core.NonEmpty Types.CsvContentType))
ccthCsvContentTypes = Lens.field @"csvContentTypes"
{-# DEPRECATED ccthCsvContentTypes "Use generic-lens or generic-optics with 'csvContentTypes' instead." #-}

-- |
--
-- /Note:/ Consider using 'jsonContentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccthJsonContentTypes :: Lens.Lens' CaptureContentTypeHeader (Core.Maybe (Core.NonEmpty Types.JsonContentType))
ccthJsonContentTypes = Lens.field @"jsonContentTypes"
{-# DEPRECATED ccthJsonContentTypes "Use generic-lens or generic-optics with 'jsonContentTypes' instead." #-}

instance Core.FromJSON CaptureContentTypeHeader where
  toJSON CaptureContentTypeHeader {..} =
    Core.object
      ( Core.catMaybes
          [ ("CsvContentTypes" Core..=) Core.<$> csvContentTypes,
            ("JsonContentTypes" Core..=) Core.<$> jsonContentTypes
          ]
      )

instance Core.FromJSON CaptureContentTypeHeader where
  parseJSON =
    Core.withObject "CaptureContentTypeHeader" Core.$
      \x ->
        CaptureContentTypeHeader'
          Core.<$> (x Core..:? "CsvContentTypes")
          Core.<*> (x Core..:? "JsonContentTypes")
