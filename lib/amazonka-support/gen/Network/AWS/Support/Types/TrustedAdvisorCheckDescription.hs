{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.TrustedAdvisorCheckDescription
  ( TrustedAdvisorCheckDescription (..),

    -- * Smart constructor
    mkTrustedAdvisorCheckDescription,

    -- * Lenses
    tacdId,
    tacdName,
    tacdDescription,
    tacdCategory,
    tacdMetadata,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.String as Types

-- | The description and metadata for a Trusted Advisor check.
--
-- /See:/ 'mkTrustedAdvisorCheckDescription' smart constructor.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription'
  { -- | The unique identifier for the Trusted Advisor check.
    id :: Types.String,
    -- | The display name for the Trusted Advisor check.
    name :: Types.String,
    -- | The description of the Trusted Advisor check, which includes the alert criteria and recommended operations (contains HTML markup).
    description :: Types.String,
    -- | The category of the Trusted Advisor check.
    category :: Types.String,
    -- | The column headings for the data returned by the Trusted Advisor check. The order of the headings corresponds to the order of the data in the __Metadata__ element of the 'TrustedAdvisorResourceDetail' for the check. __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
    metadata :: [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCheckDescription' value with any optional fields omitted.
mkTrustedAdvisorCheckDescription ::
  -- | 'id'
  Types.String ->
  -- | 'name'
  Types.String ->
  -- | 'description'
  Types.String ->
  -- | 'category'
  Types.String ->
  TrustedAdvisorCheckDescription
mkTrustedAdvisorCheckDescription id name description category =
  TrustedAdvisorCheckDescription'
    { id,
      name,
      description,
      category,
      metadata = Core.mempty
    }

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdId :: Lens.Lens' TrustedAdvisorCheckDescription Types.String
tacdId = Lens.field @"id"
{-# DEPRECATED tacdId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The display name for the Trusted Advisor check.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdName :: Lens.Lens' TrustedAdvisorCheckDescription Types.String
tacdName = Lens.field @"name"
{-# DEPRECATED tacdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The description of the Trusted Advisor check, which includes the alert criteria and recommended operations (contains HTML markup).
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdDescription :: Lens.Lens' TrustedAdvisorCheckDescription Types.String
tacdDescription = Lens.field @"description"
{-# DEPRECATED tacdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The category of the Trusted Advisor check.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdCategory :: Lens.Lens' TrustedAdvisorCheckDescription Types.String
tacdCategory = Lens.field @"category"
{-# DEPRECATED tacdCategory "Use generic-lens or generic-optics with 'category' instead." #-}

-- | The column headings for the data returned by the Trusted Advisor check. The order of the headings corresponds to the order of the data in the __Metadata__ element of the 'TrustedAdvisorResourceDetail' for the check. __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdMetadata :: Lens.Lens' TrustedAdvisorCheckDescription [Types.String]
tacdMetadata = Lens.field @"metadata"
{-# DEPRECATED tacdMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

instance Core.FromJSON TrustedAdvisorCheckDescription where
  parseJSON =
    Core.withObject "TrustedAdvisorCheckDescription" Core.$
      \x ->
        TrustedAdvisorCheckDescription'
          Core.<$> (x Core..: "id")
          Core.<*> (x Core..: "name")
          Core.<*> (x Core..: "description")
          Core.<*> (x Core..: "category")
          Core.<*> (x Core..:? "metadata" Core..!= Core.mempty)
