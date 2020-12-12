{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulItem
  ( UnsuccessfulItem (..),

    -- * Smart constructor
    mkUnsuccessfulItem,

    -- * Lenses
    uiResourceId,
    uiError,
  )
where

import Network.AWS.EC2.Types.UnsuccessfulItemError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about items that were not successfully processed in a batch call.
--
-- /See:/ 'mkUnsuccessfulItem' smart constructor.
data UnsuccessfulItem = UnsuccessfulItem'
  { resourceId ::
      Lude.Maybe Lude.Text,
    error :: Lude.Maybe UnsuccessfulItemError
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsuccessfulItem' with the minimum fields required to make a request.
--
-- * 'error' - Information about the error.
-- * 'resourceId' - The ID of the resource.
mkUnsuccessfulItem ::
  UnsuccessfulItem
mkUnsuccessfulItem =
  UnsuccessfulItem'
    { resourceId = Lude.Nothing,
      error = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiResourceId :: Lens.Lens' UnsuccessfulItem (Lude.Maybe Lude.Text)
uiResourceId = Lens.lens (resourceId :: UnsuccessfulItem -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: UnsuccessfulItem)
{-# DEPRECATED uiResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | Information about the error.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiError :: Lens.Lens' UnsuccessfulItem (Lude.Maybe UnsuccessfulItemError)
uiError = Lens.lens (error :: UnsuccessfulItem -> Lude.Maybe UnsuccessfulItemError) (\s a -> s {error = a} :: UnsuccessfulItem)
{-# DEPRECATED uiError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Lude.FromXML UnsuccessfulItem where
  parseXML x =
    UnsuccessfulItem'
      Lude.<$> (x Lude..@? "resourceId") Lude.<*> (x Lude..@? "error")
