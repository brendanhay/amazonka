{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ErrorDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ErrorDetail
  ( ErrorDetail (..),

    -- * Smart constructor
    mkErrorDetail,

    -- * Lenses
    edDetailedErrorCode,
    edDetailedErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about the error.
--
-- /See:/ 'mkErrorDetail' smart constructor.
data ErrorDetail = ErrorDetail'
  { -- | A detailed error code.
    detailedErrorCode :: Core.Maybe Core.Text,
    -- | A detailed error message.
    detailedErrorMessage :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ErrorDetail' value with any optional fields omitted.
mkErrorDetail ::
  ErrorDetail
mkErrorDetail =
  ErrorDetail'
    { detailedErrorCode = Core.Nothing,
      detailedErrorMessage = Core.Nothing
    }

-- | A detailed error code.
--
-- /Note:/ Consider using 'detailedErrorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDetailedErrorCode :: Lens.Lens' ErrorDetail (Core.Maybe Core.Text)
edDetailedErrorCode = Lens.field @"detailedErrorCode"
{-# DEPRECATED edDetailedErrorCode "Use generic-lens or generic-optics with 'detailedErrorCode' instead." #-}

-- | A detailed error message.
--
-- /Note:/ Consider using 'detailedErrorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edDetailedErrorMessage :: Lens.Lens' ErrorDetail (Core.Maybe Core.Text)
edDetailedErrorMessage = Lens.field @"detailedErrorMessage"
{-# DEPRECATED edDetailedErrorMessage "Use generic-lens or generic-optics with 'detailedErrorMessage' instead." #-}

instance Core.FromJSON ErrorDetail where
  parseJSON =
    Core.withObject "ErrorDetail" Core.$
      \x ->
        ErrorDetail'
          Core.<$> (x Core..:? "DetailedErrorCode")
          Core.<*> (x Core..:? "DetailedErrorMessage")
