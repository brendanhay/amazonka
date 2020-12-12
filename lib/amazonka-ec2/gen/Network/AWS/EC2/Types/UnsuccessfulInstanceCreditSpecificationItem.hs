{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItem
  ( UnsuccessfulInstanceCreditSpecificationItem (..),

    -- * Smart constructor
    mkUnsuccessfulInstanceCreditSpecificationItem,

    -- * Lenses
    uicsiInstanceId,
    uicsiError,
  )
where

import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the burstable performance instance whose credit option for CPU usage was not modified.
--
-- /See:/ 'mkUnsuccessfulInstanceCreditSpecificationItem' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItem = UnsuccessfulInstanceCreditSpecificationItem'
  { instanceId ::
      Lude.Maybe
        Lude.Text,
    error ::
      Lude.Maybe
        UnsuccessfulInstanceCreditSpecificationItemError
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsuccessfulInstanceCreditSpecificationItem' with the minimum fields required to make a request.
--
-- * 'error' - The applicable error for the burstable performance instance whose credit option for CPU usage was not modified.
-- * 'instanceId' - The ID of the instance.
mkUnsuccessfulInstanceCreditSpecificationItem ::
  UnsuccessfulInstanceCreditSpecificationItem
mkUnsuccessfulInstanceCreditSpecificationItem =
  UnsuccessfulInstanceCreditSpecificationItem'
    { instanceId =
        Lude.Nothing,
      error = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsiInstanceId :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Lude.Maybe Lude.Text)
uicsiInstanceId = Lens.lens (instanceId :: UnsuccessfulInstanceCreditSpecificationItem -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: UnsuccessfulInstanceCreditSpecificationItem)
{-# DEPRECATED uicsiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The applicable error for the burstable performance instance whose credit option for CPU usage was not modified.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsiError :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItem (Lude.Maybe UnsuccessfulInstanceCreditSpecificationItemError)
uicsiError = Lens.lens (error :: UnsuccessfulInstanceCreditSpecificationItem -> Lude.Maybe UnsuccessfulInstanceCreditSpecificationItemError) (\s a -> s {error = a} :: UnsuccessfulInstanceCreditSpecificationItem)
{-# DEPRECATED uicsiError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Lude.FromXML UnsuccessfulInstanceCreditSpecificationItem where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItem'
      Lude.<$> (x Lude..@? "instanceId") Lude.<*> (x Lude..@? "error")
