{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetErrorItem
  ( DeleteFleetErrorItem (..),

    -- * Smart constructor
    mkDeleteFleetErrorItem,

    -- * Lenses
    dfeiError,
    dfeiFleetId,
  )
where

import Network.AWS.EC2.Types.DeleteFleetError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 Fleet that was not successfully deleted.
--
-- /See:/ 'mkDeleteFleetErrorItem' smart constructor.
data DeleteFleetErrorItem = DeleteFleetErrorItem'
  { error ::
      Lude.Maybe DeleteFleetError,
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFleetErrorItem' with the minimum fields required to make a request.
--
-- * 'error' - The error.
-- * 'fleetId' - The ID of the EC2 Fleet.
mkDeleteFleetErrorItem ::
  DeleteFleetErrorItem
mkDeleteFleetErrorItem =
  DeleteFleetErrorItem'
    { error = Lude.Nothing,
      fleetId = Lude.Nothing
    }

-- | The error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeiError :: Lens.Lens' DeleteFleetErrorItem (Lude.Maybe DeleteFleetError)
dfeiError = Lens.lens (error :: DeleteFleetErrorItem -> Lude.Maybe DeleteFleetError) (\s a -> s {error = a} :: DeleteFleetErrorItem)
{-# DEPRECATED dfeiError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The ID of the EC2 Fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeiFleetId :: Lens.Lens' DeleteFleetErrorItem (Lude.Maybe Lude.Text)
dfeiFleetId = Lens.lens (fleetId :: DeleteFleetErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DeleteFleetErrorItem)
{-# DEPRECATED dfeiFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.FromXML DeleteFleetErrorItem where
  parseXML x =
    DeleteFleetErrorItem'
      Lude.<$> (x Lude..@? "error") Lude.<*> (x Lude..@? "fleetId")
