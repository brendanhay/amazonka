-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.UpdateAttributesRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.UpdateAttributesRequest
  ( UpdateAttributesRequest (..),

    -- * Smart constructor
    mkUpdateAttributesRequest,

    -- * Lenses
    uarBlacklist,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies one or more attributes to remove from all the endpoints that are associated with an application.
--
-- /See:/ 'mkUpdateAttributesRequest' smart constructor.
newtype UpdateAttributesRequest = UpdateAttributesRequest'
  { blacklist ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAttributesRequest' with the minimum fields required to make a request.
--
-- * 'blacklist' - An array of the attributes to remove from all the endpoints that are associated with the application. The array can specify the complete, exact name of each attribute to remove or it can specify a glob pattern that an attribute name must match in order for the attribute to be removed.
mkUpdateAttributesRequest ::
  UpdateAttributesRequest
mkUpdateAttributesRequest =
  UpdateAttributesRequest' {blacklist = Lude.Nothing}

-- | An array of the attributes to remove from all the endpoints that are associated with the application. The array can specify the complete, exact name of each attribute to remove or it can specify a glob pattern that an attribute name must match in order for the attribute to be removed.
--
-- /Note:/ Consider using 'blacklist' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarBlacklist :: Lens.Lens' UpdateAttributesRequest (Lude.Maybe [Lude.Text])
uarBlacklist = Lens.lens (blacklist :: UpdateAttributesRequest -> Lude.Maybe [Lude.Text]) (\s a -> s {blacklist = a} :: UpdateAttributesRequest)
{-# DEPRECATED uarBlacklist "Use generic-lens or generic-optics with 'blacklist' instead." #-}

instance Lude.ToJSON UpdateAttributesRequest where
  toJSON UpdateAttributesRequest' {..} =
    Lude.object
      (Lude.catMaybes [("Blacklist" Lude..=) Lude.<$> blacklist])
