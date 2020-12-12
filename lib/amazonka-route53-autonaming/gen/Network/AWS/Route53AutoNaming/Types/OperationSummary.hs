{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationSummary
  ( OperationSummary (..),

    -- * Smart constructor
    mkOperationSummary,

    -- * Lenses
    osStatus,
    osId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.OperationStatus

-- | A complex type that contains information about an operation that matches the criteria that you specified in a <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListOperations.html ListOperations> request.
--
-- /See:/ 'mkOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { status ::
      Lude.Maybe OperationStatus,
    id :: Lude.Maybe Lude.Text
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
-- * 'id' - The ID for an operation.
-- * 'status' - The status of the operation. Values include the following:
--
--
--     * __SUBMITTED__ : This is the initial state immediately after you submit a request.
--
--
--     * __PENDING__ : AWS Cloud Map is performing the operation.
--
--
--     * __SUCCESS__ : The operation succeeded.
--
--
--     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
mkOperationSummary ::
  OperationSummary
mkOperationSummary =
  OperationSummary' {status = Lude.Nothing, id = Lude.Nothing}

-- | The status of the operation. Values include the following:
--
--
--     * __SUBMITTED__ : This is the initial state immediately after you submit a request.
--
--
--     * __PENDING__ : AWS Cloud Map is performing the operation.
--
--
--     * __SUCCESS__ : The operation succeeded.
--
--
--     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osStatus :: Lens.Lens' OperationSummary (Lude.Maybe OperationStatus)
osStatus = Lens.lens (status :: OperationSummary -> Lude.Maybe OperationStatus) (\s a -> s {status = a} :: OperationSummary)
{-# DEPRECATED osStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID for an operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osId :: Lens.Lens' OperationSummary (Lude.Maybe Lude.Text)
osId = Lens.lens (id :: OperationSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: OperationSummary)
{-# DEPRECATED osId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON OperationSummary where
  parseJSON =
    Lude.withObject
      "OperationSummary"
      ( \x ->
          OperationSummary'
            Lude.<$> (x Lude..:? "Status") Lude.<*> (x Lude..:? "Id")
      )
