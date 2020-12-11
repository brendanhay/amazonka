-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ResourceCountFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ResourceCountFilters
  ( ResourceCountFilters (..),

    -- * Smart constructor
    mkResourceCountFilters,

    -- * Lenses
    rcfResourceType,
    rcfAccountId,
    rcfRegion,
  )
where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filters the resource count based on account ID, region, and resource type.
--
-- /See:/ 'mkResourceCountFilters' smart constructor.
data ResourceCountFilters = ResourceCountFilters'
  { resourceType ::
      Lude.Maybe ResourceType,
    accountId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceCountFilters' with the minimum fields required to make a request.
--
-- * 'accountId' - The 12-digit ID of the account.
-- * 'region' - The region where the account is located.
-- * 'resourceType' - The type of the AWS resource.
mkResourceCountFilters ::
  ResourceCountFilters
mkResourceCountFilters =
  ResourceCountFilters'
    { resourceType = Lude.Nothing,
      accountId = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The type of the AWS resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfResourceType :: Lens.Lens' ResourceCountFilters (Lude.Maybe ResourceType)
rcfResourceType = Lens.lens (resourceType :: ResourceCountFilters -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: ResourceCountFilters)
{-# DEPRECATED rcfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The 12-digit ID of the account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfAccountId :: Lens.Lens' ResourceCountFilters (Lude.Maybe Lude.Text)
rcfAccountId = Lens.lens (accountId :: ResourceCountFilters -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: ResourceCountFilters)
{-# DEPRECATED rcfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The region where the account is located.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcfRegion :: Lens.Lens' ResourceCountFilters (Lude.Maybe Lude.Text)
rcfRegion = Lens.lens (region :: ResourceCountFilters -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ResourceCountFilters)
{-# DEPRECATED rcfRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.ToJSON ResourceCountFilters where
  toJSON ResourceCountFilters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ResourceType" Lude..=) Lude.<$> resourceType,
            ("AccountId" Lude..=) Lude.<$> accountId,
            ("Region" Lude..=) Lude.<$> region
          ]
      )
