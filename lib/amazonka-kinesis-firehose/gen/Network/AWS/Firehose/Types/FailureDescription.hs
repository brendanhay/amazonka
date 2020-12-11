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

import Network.AWS.Firehose.Types.DeliveryStreamFailureType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- /See:/ 'mkFailureDescription' smart constructor.
data FailureDescription = FailureDescription'
  { type' ::
      DeliveryStreamFailureType,
    details :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailureDescription' with the minimum fields required to make a request.
--
-- * 'details' - A message providing details about the error that caused the failure.
-- * 'type'' - The type of error that caused the failure.
mkFailureDescription ::
  -- | 'type''
  DeliveryStreamFailureType ->
  -- | 'details'
  Lude.Text ->
  FailureDescription
mkFailureDescription pType_ pDetails_ =
  FailureDescription' {type' = pType_, details = pDetails_}

-- | The type of error that caused the failure.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdType :: Lens.Lens' FailureDescription DeliveryStreamFailureType
fdType = Lens.lens (type' :: FailureDescription -> DeliveryStreamFailureType) (\s a -> s {type' = a} :: FailureDescription)
{-# DEPRECATED fdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A message providing details about the error that caused the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fdDetails :: Lens.Lens' FailureDescription Lude.Text
fdDetails = Lens.lens (details :: FailureDescription -> Lude.Text) (\s a -> s {details = a} :: FailureDescription)
{-# DEPRECATED fdDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromJSON FailureDescription where
  parseJSON =
    Lude.withObject
      "FailureDescription"
      ( \x ->
          FailureDescription'
            Lude.<$> (x Lude..: "Type") Lude.<*> (x Lude..: "Details")
      )
