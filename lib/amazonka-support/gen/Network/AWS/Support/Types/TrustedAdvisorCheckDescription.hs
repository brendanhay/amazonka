{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.TrustedAdvisorCheckDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.TrustedAdvisorCheckDescription
  ( TrustedAdvisorCheckDescription (..)
  -- * Smart constructor
  , mkTrustedAdvisorCheckDescription
  -- * Lenses
  , tacdId
  , tacdName
  , tacdDescription
  , tacdCategory
  , tacdMetadata
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The description and metadata for a Trusted Advisor check.
--
-- /See:/ 'mkTrustedAdvisorCheckDescription' smart constructor.
data TrustedAdvisorCheckDescription = TrustedAdvisorCheckDescription'
  { id :: Core.Text
    -- ^ The unique identifier for the Trusted Advisor check.
  , name :: Core.Text
    -- ^ The display name for the Trusted Advisor check.
  , description :: Core.Text
    -- ^ The description of the Trusted Advisor check, which includes the alert criteria and recommended operations (contains HTML markup).
  , category :: Core.Text
    -- ^ The category of the Trusted Advisor check.
  , metadata :: [Core.Text]
    -- ^ The column headings for the data returned by the Trusted Advisor check. The order of the headings corresponds to the order of the data in the __Metadata__ element of the 'TrustedAdvisorResourceDetail' for the check. __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrustedAdvisorCheckDescription' value with any optional fields omitted.
mkTrustedAdvisorCheckDescription
    :: Core.Text -- ^ 'id'
    -> Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'description'
    -> Core.Text -- ^ 'category'
    -> TrustedAdvisorCheckDescription
mkTrustedAdvisorCheckDescription id name description category
  = TrustedAdvisorCheckDescription'{id, name, description, category,
                                    metadata = Core.mempty}

-- | The unique identifier for the Trusted Advisor check.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdId :: Lens.Lens' TrustedAdvisorCheckDescription Core.Text
tacdId = Lens.field @"id"
{-# INLINEABLE tacdId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The display name for the Trusted Advisor check.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdName :: Lens.Lens' TrustedAdvisorCheckDescription Core.Text
tacdName = Lens.field @"name"
{-# INLINEABLE tacdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The description of the Trusted Advisor check, which includes the alert criteria and recommended operations (contains HTML markup).
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdDescription :: Lens.Lens' TrustedAdvisorCheckDescription Core.Text
tacdDescription = Lens.field @"description"
{-# INLINEABLE tacdDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The category of the Trusted Advisor check.
--
-- /Note:/ Consider using 'category' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdCategory :: Lens.Lens' TrustedAdvisorCheckDescription Core.Text
tacdCategory = Lens.field @"category"
{-# INLINEABLE tacdCategory #-}
{-# DEPRECATED category "Use generic-lens or generic-optics with 'category' instead"  #-}

-- | The column headings for the data returned by the Trusted Advisor check. The order of the headings corresponds to the order of the data in the __Metadata__ element of the 'TrustedAdvisorResourceDetail' for the check. __Metadata__ contains all the data that is shown in the Excel download, even in those cases where the UI shows just summary data. 
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tacdMetadata :: Lens.Lens' TrustedAdvisorCheckDescription [Core.Text]
tacdMetadata = Lens.field @"metadata"
{-# INLINEABLE tacdMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

instance Core.FromJSON TrustedAdvisorCheckDescription where
        parseJSON
          = Core.withObject "TrustedAdvisorCheckDescription" Core.$
              \ x ->
                TrustedAdvisorCheckDescription' Core.<$>
                  (x Core..: "id") Core.<*> x Core..: "name" Core.<*>
                    x Core..: "description"
                    Core.<*> x Core..: "category"
                    Core.<*> x Core..:? "metadata" Core..!= Core.mempty
