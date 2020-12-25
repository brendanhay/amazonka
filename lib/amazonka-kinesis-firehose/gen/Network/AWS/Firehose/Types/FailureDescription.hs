{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.FailureDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.FailureDescription
  ( FailureDescription (..),

    -- * Smart constructor
    mkFailureDescription,

    -- * Lenses
    fdType,
    fdDetails,
  )
where

import qualified Network.AWS.Firehose.Types.DeliveryStreamFailureType as Types
import qualified Network.AWS.Firehose.Types.Details as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- /See:/ 'mkFailureDescription' smart constructor.
data FailureDescription = FailureDescription'
  { -- | The type of error that caused the failure.
    type' :: Types.DeliveryStreamFailureType,
    -- | A message providing details about the error that caused the failure.
    details :: Types.Details
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailureDescription' value with any optional fields omitted.
mkFailureDescription ::
  -- | 'type\''
  Types.DeliveryStreamFailureType ->
  -- | 'details'
  Types.Details ->
  FailureDescription
mkFailureDescription type' details =
  FailureDescription' {type', details}

-- | The type of error that caused the failure.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdType :: Lens.Lens' FailureDescription Types.DeliveryStreamFailureType
fdType = Lens.field @"type'"
{-# DEPRECATED fdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message providing details about the error that caused the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdDetails :: Lens.Lens' FailureDescription Types.Details
fdDetails = Lens.field @"details"
{-# DEPRECATED fdDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Core.FromJSON FailureDescription where
  parseJSON =
    Core.withObject "FailureDescription" Core.$
      \x ->
        FailureDescription'
          Core.<$> (x Core..: "Type") Core.<*> (x Core..: "Details")
