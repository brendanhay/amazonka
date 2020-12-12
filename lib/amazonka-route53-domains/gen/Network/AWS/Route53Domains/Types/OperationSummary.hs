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
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53Domains.Types.OperationStatus
import Network.AWS.Route53Domains.Types.OperationType

-- | OperationSummary includes the following elements.
--
-- /See:/ 'mkOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { operationId :: Lude.Text,
    status :: OperationStatus,
    type' :: OperationType,
    submittedDate :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OperationSummary' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier returned to track the requested action.
-- * 'status' - The current status of the requested operation in the system.
-- * 'submittedDate' - The date when the request was submitted.
-- * 'type'' - Type of the action requested.
mkOperationSummary ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'status'
  OperationStatus ->
  -- | 'type''
  OperationType ->
  -- | 'submittedDate'
  Lude.Timestamp ->
  OperationSummary
mkOperationSummary pOperationId_ pStatus_ pType_ pSubmittedDate_ =
  OperationSummary'
    { operationId = pOperationId_,
      status = pStatus_,
      type' = pType_,
      submittedDate = pSubmittedDate_
    }

-- | Identifier returned to track the requested action.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOperationId :: Lens.Lens' OperationSummary Lude.Text
osOperationId = Lens.lens (operationId :: OperationSummary -> Lude.Text) (\s a -> s {operationId = a} :: OperationSummary)
{-# DEPRECATED osOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The current status of the requested operation in the system.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osStatus :: Lens.Lens' OperationSummary OperationStatus
osStatus = Lens.lens (status :: OperationSummary -> OperationStatus) (\s a -> s {status = a} :: OperationSummary)
{-# DEPRECATED osStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Type of the action requested.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osType :: Lens.Lens' OperationSummary OperationType
osType = Lens.lens (type' :: OperationSummary -> OperationType) (\s a -> s {type' = a} :: OperationSummary)
{-# DEPRECATED osType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date when the request was submitted.
--
-- /Note:/ Consider using 'submittedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osSubmittedDate :: Lens.Lens' OperationSummary Lude.Timestamp
osSubmittedDate = Lens.lens (submittedDate :: OperationSummary -> Lude.Timestamp) (\s a -> s {submittedDate = a} :: OperationSummary)
{-# DEPRECATED osSubmittedDate "Use generic-lens or generic-optics with 'submittedDate' instead." #-}

instance Lude.FromJSON OperationSummary where
  parseJSON =
    Lude.withObject
      "OperationSummary"
      ( \x ->
          OperationSummary'
            Lude.<$> (x Lude..: "OperationId")
            Lude.<*> (x Lude..: "Status")
            Lude.<*> (x Lude..: "Type")
            Lude.<*> (x Lude..: "SubmittedDate")
      )
