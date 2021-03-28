{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.SkillDetails
  ( SkillDetails (..)
  -- * Smart constructor
  , mkSkillDetails
  -- * Lenses
  , sdBulletPoints
  , sdDeveloperInfo
  , sdEndUserLicenseAgreement
  , sdGenericKeywords
  , sdInvocationPhrase
  , sdNewInThisVersionBulletPoints
  , sdProductDescription
  , sdReleaseDate
  , sdReviews
  , sdSkillTypes
  ) where

import qualified Network.AWS.AlexaBusiness.Types.BulletPoint as Types
import qualified Network.AWS.AlexaBusiness.Types.DeveloperInfo as Types
import qualified Network.AWS.AlexaBusiness.Types.EndUserLicenseAgreement as Types
import qualified Network.AWS.AlexaBusiness.Types.GenericKeyword as Types
import qualified Network.AWS.AlexaBusiness.Types.InvocationPhrase as Types
import qualified Network.AWS.AlexaBusiness.Types.ProductDescription as Types
import qualified Network.AWS.AlexaBusiness.Types.ReleaseDate as Types
import qualified Network.AWS.AlexaBusiness.Types.ReviewKey as Types
import qualified Network.AWS.AlexaBusiness.Types.ReviewValue as Types
import qualified Network.AWS.AlexaBusiness.Types.SkillStoreType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Granular information about the skill.
--
-- /See:/ 'mkSkillDetails' smart constructor.
data SkillDetails = SkillDetails'
  { bulletPoints :: Core.Maybe [Types.BulletPoint]
    -- ^ The details about what the skill supports organized as bullet points.
  , developerInfo :: Core.Maybe Types.DeveloperInfo
    -- ^ The details about the developer that published the skill.
  , endUserLicenseAgreement :: Core.Maybe Types.EndUserLicenseAgreement
    -- ^ The URL of the end user license agreement.
  , genericKeywords :: Core.Maybe [Types.GenericKeyword]
    -- ^ The generic keywords associated with the skill that can be used to find a skill.
  , invocationPhrase :: Core.Maybe Types.InvocationPhrase
    -- ^ The phrase used to trigger the skill.
  , newInThisVersionBulletPoints :: Core.Maybe [Types.BulletPoint]
    -- ^ The updates added in bullet points.
  , productDescription :: Core.Maybe Types.ProductDescription
    -- ^ The description of the product.
  , releaseDate :: Core.Maybe Types.ReleaseDate
    -- ^ The date when the skill was released.
  , reviews :: Core.Maybe (Core.HashMap Types.ReviewKey Types.ReviewValue)
    -- ^ /This member has been deprecated./ 
--
-- The list of reviews for the skill, including Key and Value pair.
  , skillTypes :: Core.Maybe [Types.SkillStoreType]
    -- ^ The types of skills.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SkillDetails' value with any optional fields omitted.
mkSkillDetails
    :: SkillDetails
mkSkillDetails
  = SkillDetails'{bulletPoints = Core.Nothing,
                  developerInfo = Core.Nothing,
                  endUserLicenseAgreement = Core.Nothing,
                  genericKeywords = Core.Nothing, invocationPhrase = Core.Nothing,
                  newInThisVersionBulletPoints = Core.Nothing,
                  productDescription = Core.Nothing, releaseDate = Core.Nothing,
                  reviews = Core.Nothing, skillTypes = Core.Nothing}

-- | The details about what the skill supports organized as bullet points.
--
-- /Note:/ Consider using 'bulletPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBulletPoints :: Lens.Lens' SkillDetails (Core.Maybe [Types.BulletPoint])
sdBulletPoints = Lens.field @"bulletPoints"
{-# INLINEABLE sdBulletPoints #-}
{-# DEPRECATED bulletPoints "Use generic-lens or generic-optics with 'bulletPoints' instead"  #-}

-- | The details about the developer that published the skill.
--
-- /Note:/ Consider using 'developerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeveloperInfo :: Lens.Lens' SkillDetails (Core.Maybe Types.DeveloperInfo)
sdDeveloperInfo = Lens.field @"developerInfo"
{-# INLINEABLE sdDeveloperInfo #-}
{-# DEPRECATED developerInfo "Use generic-lens or generic-optics with 'developerInfo' instead"  #-}

-- | The URL of the end user license agreement.
--
-- /Note:/ Consider using 'endUserLicenseAgreement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEndUserLicenseAgreement :: Lens.Lens' SkillDetails (Core.Maybe Types.EndUserLicenseAgreement)
sdEndUserLicenseAgreement = Lens.field @"endUserLicenseAgreement"
{-# INLINEABLE sdEndUserLicenseAgreement #-}
{-# DEPRECATED endUserLicenseAgreement "Use generic-lens or generic-optics with 'endUserLicenseAgreement' instead"  #-}

-- | The generic keywords associated with the skill that can be used to find a skill.
--
-- /Note:/ Consider using 'genericKeywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdGenericKeywords :: Lens.Lens' SkillDetails (Core.Maybe [Types.GenericKeyword])
sdGenericKeywords = Lens.field @"genericKeywords"
{-# INLINEABLE sdGenericKeywords #-}
{-# DEPRECATED genericKeywords "Use generic-lens or generic-optics with 'genericKeywords' instead"  #-}

-- | The phrase used to trigger the skill.
--
-- /Note:/ Consider using 'invocationPhrase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdInvocationPhrase :: Lens.Lens' SkillDetails (Core.Maybe Types.InvocationPhrase)
sdInvocationPhrase = Lens.field @"invocationPhrase"
{-# INLINEABLE sdInvocationPhrase #-}
{-# DEPRECATED invocationPhrase "Use generic-lens or generic-optics with 'invocationPhrase' instead"  #-}

-- | The updates added in bullet points.
--
-- /Note:/ Consider using 'newInThisVersionBulletPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdNewInThisVersionBulletPoints :: Lens.Lens' SkillDetails (Core.Maybe [Types.BulletPoint])
sdNewInThisVersionBulletPoints = Lens.field @"newInThisVersionBulletPoints"
{-# INLINEABLE sdNewInThisVersionBulletPoints #-}
{-# DEPRECATED newInThisVersionBulletPoints "Use generic-lens or generic-optics with 'newInThisVersionBulletPoints' instead"  #-}

-- | The description of the product.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdProductDescription :: Lens.Lens' SkillDetails (Core.Maybe Types.ProductDescription)
sdProductDescription = Lens.field @"productDescription"
{-# INLINEABLE sdProductDescription #-}
{-# DEPRECATED productDescription "Use generic-lens or generic-optics with 'productDescription' instead"  #-}

-- | The date when the skill was released.
--
-- /Note:/ Consider using 'releaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdReleaseDate :: Lens.Lens' SkillDetails (Core.Maybe Types.ReleaseDate)
sdReleaseDate = Lens.field @"releaseDate"
{-# INLINEABLE sdReleaseDate #-}
{-# DEPRECATED releaseDate "Use generic-lens or generic-optics with 'releaseDate' instead"  #-}

-- | /This member has been deprecated./ 
--
-- The list of reviews for the skill, including Key and Value pair.
--
-- /Note:/ Consider using 'reviews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdReviews :: Lens.Lens' SkillDetails (Core.Maybe (Core.HashMap Types.ReviewKey Types.ReviewValue))
sdReviews = Lens.field @"reviews"
{-# INLINEABLE sdReviews #-}
{-# DEPRECATED reviews "Use generic-lens or generic-optics with 'reviews' instead"  #-}

-- | The types of skills.
--
-- /Note:/ Consider using 'skillTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSkillTypes :: Lens.Lens' SkillDetails (Core.Maybe [Types.SkillStoreType])
sdSkillTypes = Lens.field @"skillTypes"
{-# INLINEABLE sdSkillTypes #-}
{-# DEPRECATED skillTypes "Use generic-lens or generic-optics with 'skillTypes' instead"  #-}

instance Core.FromJSON SkillDetails where
        parseJSON
          = Core.withObject "SkillDetails" Core.$
              \ x ->
                SkillDetails' Core.<$>
                  (x Core..:? "BulletPoints") Core.<*> x Core..:? "DeveloperInfo"
                    Core.<*> x Core..:? "EndUserLicenseAgreement"
                    Core.<*> x Core..:? "GenericKeywords"
                    Core.<*> x Core..:? "InvocationPhrase"
                    Core.<*> x Core..:? "NewInThisVersionBulletPoints"
                    Core.<*> x Core..:? "ProductDescription"
                    Core.<*> x Core..:? "ReleaseDate"
                    Core.<*> x Core..:? "Reviews"
                    Core.<*> x Core..:? "SkillTypes"
