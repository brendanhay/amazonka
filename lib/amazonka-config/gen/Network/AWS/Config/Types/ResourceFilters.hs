{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceFilters
  ( ResourceFilters (..),

    -- * Smart constructor
    mkResourceFilters,

    -- * Lenses
    rfResourceId,
    rfResourceName,
    rfAccountId,
    rfRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters the results by resource account ID, region, resource ID, and resource name.
--
-- /See:/ 'mkResourceFilters' smart constructor.
data ResourceFilters = ResourceFilters'
  { -- | The ID of the resource.
    resourceId :: Lude.Maybe Lude.Text,
    -- | The name of the resource.
    resourceName :: Lude.Maybe Lude.Text,
    -- | The 12-digit source account ID.
    accountId :: Lude.Maybe Lude.Text,
    -- | The source region.
    region :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceFilters' with the minimum fields required to make a request.
--
-- * 'resourceId' - The ID of the resource.
-- * 'resourceName' - The name of the resource.
-- * 'accountId' - The 12-digit source account ID.
-- * 'region' - The source region.
mkResourceFilters ::
  ResourceFilters
mkResourceFilters =
  ResourceFilters'
    { resourceId = Lude.Nothing,
      resourceName = Lude.Nothing,
      accountId = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The ID of the resource.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfResourceId :: Lens.Lens' ResourceFilters (Lude.Maybe Lude.Text)
rfResourceId = Lens.lens (resourceId :: ResourceFilters -> Lude.Maybe Lude.Text) (\s a -> s {resourceId = a} :: ResourceFilters)
{-# DEPRECATED rfResourceId "Use generic-lens or generic-optics with 'resourceId' instead." #-}

-- | The name of the resource.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfResourceName :: Lens.Lens' ResourceFilters (Lude.Maybe Lude.Text)
rfResourceName = Lens.lens (resourceName :: ResourceFilters -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: ResourceFilters)
{-# DEPRECATED rfResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The 12-digit source account ID.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfAccountId :: Lens.Lens' ResourceFilters (Lude.Maybe Lude.Text)
rfAccountId = Lens.lens (accountId :: ResourceFilters -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ResourceFilters)
{-# DEPRECATED rfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The source region.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfRegion :: Lens.Lens' ResourceFilters (Lude.Maybe Lude.Text)
rfRegion = Lens.lens (region :: ResourceFilters -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ResourceFilters)
{-# DEPRECATED rfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.ToJSON ResourceFilters where
  toJSON ResourceFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceId" Lude..=) Lude.<$> resourceId,
            ("ResourceName" Lude..=) Lude.<$> resourceName,
            ("AccountId" Lude..=) Lude.<$> accountId,
            ("Region" Lude..=) Lude.<$> region
          ]
      )
