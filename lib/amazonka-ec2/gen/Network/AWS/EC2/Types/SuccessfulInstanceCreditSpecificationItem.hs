{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SuccessfulInstanceCreditSpecificationItem
  ( SuccessfulInstanceCreditSpecificationItem (..),

    -- * Smart constructor
    mkSuccessfulInstanceCreditSpecificationItem,

    -- * Lenses
    sicsiInstanceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the burstable performance instance whose credit option for CPU usage was successfully modified.
--
-- /See:/ 'mkSuccessfulInstanceCreditSpecificationItem' smart constructor.
newtype SuccessfulInstanceCreditSpecificationItem = SuccessfulInstanceCreditSpecificationItem'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SuccessfulInstanceCreditSpecificationItem' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
mkSuccessfulInstanceCreditSpecificationItem ::
  SuccessfulInstanceCreditSpecificationItem
mkSuccessfulInstanceCreditSpecificationItem =
  SuccessfulInstanceCreditSpecificationItem'
    { instanceId =
        Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sicsiInstanceId :: Lens.Lens' SuccessfulInstanceCreditSpecificationItem (Lude.Maybe Lude.Text)
sicsiInstanceId = Lens.lens (instanceId :: SuccessfulInstanceCreditSpecificationItem -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: SuccessfulInstanceCreditSpecificationItem)
{-# DEPRECATED sicsiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.FromXML SuccessfulInstanceCreditSpecificationItem where
  parseXML x =
    SuccessfulInstanceCreditSpecificationItem'
      Lude.<$> (x Lude..@? "instanceId")
