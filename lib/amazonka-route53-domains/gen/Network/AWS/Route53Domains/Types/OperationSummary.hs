{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.OperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.OperationSummary
  ( OperationSummary (..),

    -- * Smart constructor
    mkOperationSummary,

    -- * Lenses
    osOperationId,
    osStatus,
    osType,
    osSubmittedDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53Domains.Types.OperationId as Types
import qualified Network.AWS.Route53Domains.Types.OperationStatus as Types
import qualified Network.AWS.Route53Domains.Types.OperationType as Types

-- | OperationSummary includes the following elements.
--
-- /See:/ 'mkOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { -- | Identifier returned to track the requested action.
    operationId :: Types.OperationId,
    -- | The current status of the requested operation in the system.
    status :: Types.OperationStatus,
    -- | Type of the action requested.
    type' :: Types.OperationType,
    -- | The date when the request was submitted.
    submittedDate :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'OperationSummary' value with any optional fields omitted.
mkOperationSummary ::
  -- | 'operationId'
  Types.OperationId ->
  -- | 'status'
  Types.OperationStatus ->
  -- | 'type\''
  Types.OperationType ->
  -- | 'submittedDate'
  Core.NominalDiffTime ->
  OperationSummary
mkOperationSummary operationId status type' submittedDate =
  OperationSummary' {operationId, status, type', submittedDate}

-- | Identifier returned to track the requested action.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOperationId :: Lens.Lens' OperationSummary Types.OperationId
osOperationId = Lens.field @"operationId"
{-# DEPRECATED osOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The current status of the requested operation in the system.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osStatus :: Lens.Lens' OperationSummary Types.OperationStatus
osStatus = Lens.field @"status"
{-# DEPRECATED osStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Type of the action requested.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperationSummary Types.OperationType
osType = Lens.field @"type'"
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date when the request was submitted.
--
-- /Note:/ Consider using 'submittedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSubmittedDate :: Lens.Lens' OperationSummary Core.NominalDiffTime
osSubmittedDate = Lens.field @"submittedDate"
{-# DEPRECATED osSubmittedDate "Use generic-lens or generic-optics with 'submittedDate' instead." #-}

instance Core.FromJSON OperationSummary where
  parseJSON =
    Core.withObject "OperationSummary" Core.$
      \x ->
        OperationSummary'
          Core.<$> (x Core..: "OperationId")
          Core.<*> (x Core..: "Status")
          Core.<*> (x Core..: "Type")
          Core.<*> (x Core..: "SubmittedDate")
