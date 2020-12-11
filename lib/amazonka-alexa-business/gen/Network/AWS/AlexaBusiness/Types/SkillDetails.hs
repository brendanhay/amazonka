-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillDetails
  ( SkillDetails (..),

    -- * Smart constructor
    mkSkillDetails,

    -- * Lenses
    sdSkillTypes,
    sdProductDescription,
    sdInvocationPhrase,
    sdDeveloperInfo,
    sdEndUserLicenseAgreement,
    sdGenericKeywords,
    sdReviews,
    sdReleaseDate,
    sdNewInThisVersionBulletPoints,
    sdBulletPoints,
  )
where

import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Granular information about the skill.
--
-- /See:/ 'mkSkillDetails' smart constructor.
data SkillDetails = SkillDetails'
  { skillTypes ::
      Lude.Maybe [Lude.Text],
    productDescription :: Lude.Maybe Lude.Text,
    invocationPhrase :: Lude.Maybe Lude.Text,
    developerInfo :: Lude.Maybe DeveloperInfo,
    endUserLicenseAgreement :: Lude.Maybe Lude.Text,
    genericKeywords :: Lude.Maybe [Lude.Text],
    reviews :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    releaseDate :: Lude.Maybe Lude.Text,
    newInThisVersionBulletPoints :: Lude.Maybe [Lude.Text],
    bulletPoints :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SkillDetails' with the minimum fields required to make a request.
--
-- * 'bulletPoints' - The details about what the skill supports organized as bullet points.
-- * 'developerInfo' - The details about the developer that published the skill.
-- * 'endUserLicenseAgreement' - The URL of the end user license agreement.
-- * 'genericKeywords' - The generic keywords associated with the skill that can be used to find a skill.
-- * 'invocationPhrase' - The phrase used to trigger the skill.
-- * 'newInThisVersionBulletPoints' - The updates added in bullet points.
-- * 'productDescription' - The description of the product.
-- * 'releaseDate' - The date when the skill was released.
-- * 'reviews' - /This member has been deprecated./
--
-- The list of reviews for the skill, including Key and Value pair.
-- * 'skillTypes' - The types of skills.
mkSkillDetails ::
  SkillDetails
mkSkillDetails =
  SkillDetails'
    { skillTypes = Lude.Nothing,
      productDescription = Lude.Nothing,
      invocationPhrase = Lude.Nothing,
      developerInfo = Lude.Nothing,
      endUserLicenseAgreement = Lude.Nothing,
      genericKeywords = Lude.Nothing,
      reviews = Lude.Nothing,
      releaseDate = Lude.Nothing,
      newInThisVersionBulletPoints = Lude.Nothing,
      bulletPoints = Lude.Nothing
    }

-- | The types of skills.
--
-- /Note:/ Consider using 'skillTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSkillTypes :: Lens.Lens' SkillDetails (Lude.Maybe [Lude.Text])
sdSkillTypes = Lens.lens (skillTypes :: SkillDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {skillTypes = a} :: SkillDetails)
{-# DEPRECATED sdSkillTypes "Use generic-lens or generic-optics with 'skillTypes' instead." #-}

-- | The description of the product.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdProductDescription :: Lens.Lens' SkillDetails (Lude.Maybe Lude.Text)
sdProductDescription = Lens.lens (productDescription :: SkillDetails -> Lude.Maybe Lude.Text) (\s a -> s {productDescription = a} :: SkillDetails)
{-# DEPRECATED sdProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The phrase used to trigger the skill.
--
-- /Note:/ Consider using 'invocationPhrase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdInvocationPhrase :: Lens.Lens' SkillDetails (Lude.Maybe Lude.Text)
sdInvocationPhrase = Lens.lens (invocationPhrase :: SkillDetails -> Lude.Maybe Lude.Text) (\s a -> s {invocationPhrase = a} :: SkillDetails)
{-# DEPRECATED sdInvocationPhrase "Use generic-lens or generic-optics with 'invocationPhrase' instead." #-}

-- | The details about the developer that published the skill.
--
-- /Note:/ Consider using 'developerInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeveloperInfo :: Lens.Lens' SkillDetails (Lude.Maybe DeveloperInfo)
sdDeveloperInfo = Lens.lens (developerInfo :: SkillDetails -> Lude.Maybe DeveloperInfo) (\s a -> s {developerInfo = a} :: SkillDetails)
{-# DEPRECATED sdDeveloperInfo "Use generic-lens or generic-optics with 'developerInfo' instead." #-}

-- | The URL of the end user license agreement.
--
-- /Note:/ Consider using 'endUserLicenseAgreement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdEndUserLicenseAgreement :: Lens.Lens' SkillDetails (Lude.Maybe Lude.Text)
sdEndUserLicenseAgreement = Lens.lens (endUserLicenseAgreement :: SkillDetails -> Lude.Maybe Lude.Text) (\s a -> s {endUserLicenseAgreement = a} :: SkillDetails)
{-# DEPRECATED sdEndUserLicenseAgreement "Use generic-lens or generic-optics with 'endUserLicenseAgreement' instead." #-}

-- | The generic keywords associated with the skill that can be used to find a skill.
--
-- /Note:/ Consider using 'genericKeywords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdGenericKeywords :: Lens.Lens' SkillDetails (Lude.Maybe [Lude.Text])
sdGenericKeywords = Lens.lens (genericKeywords :: SkillDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {genericKeywords = a} :: SkillDetails)
{-# DEPRECATED sdGenericKeywords "Use generic-lens or generic-optics with 'genericKeywords' instead." #-}

-- | /This member has been deprecated./
--
-- The list of reviews for the skill, including Key and Value pair.
--
-- /Note:/ Consider using 'reviews' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdReviews :: Lens.Lens' SkillDetails (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
sdReviews = Lens.lens (reviews :: SkillDetails -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {reviews = a} :: SkillDetails)
{-# DEPRECATED sdReviews "Use generic-lens or generic-optics with 'reviews' instead." #-}

-- | The date when the skill was released.
--
-- /Note:/ Consider using 'releaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdReleaseDate :: Lens.Lens' SkillDetails (Lude.Maybe Lude.Text)
sdReleaseDate = Lens.lens (releaseDate :: SkillDetails -> Lude.Maybe Lude.Text) (\s a -> s {releaseDate = a} :: SkillDetails)
{-# DEPRECATED sdReleaseDate "Use generic-lens or generic-optics with 'releaseDate' instead." #-}

-- | The updates added in bullet points.
--
-- /Note:/ Consider using 'newInThisVersionBulletPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdNewInThisVersionBulletPoints :: Lens.Lens' SkillDetails (Lude.Maybe [Lude.Text])
sdNewInThisVersionBulletPoints = Lens.lens (newInThisVersionBulletPoints :: SkillDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {newInThisVersionBulletPoints = a} :: SkillDetails)
{-# DEPRECATED sdNewInThisVersionBulletPoints "Use generic-lens or generic-optics with 'newInThisVersionBulletPoints' instead." #-}

-- | The details about what the skill supports organized as bullet points.
--
-- /Note:/ Consider using 'bulletPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBulletPoints :: Lens.Lens' SkillDetails (Lude.Maybe [Lude.Text])
sdBulletPoints = Lens.lens (bulletPoints :: SkillDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {bulletPoints = a} :: SkillDetails)
{-# DEPRECATED sdBulletPoints "Use generic-lens or generic-optics with 'bulletPoints' instead." #-}

instance Lude.FromJSON SkillDetails where
  parseJSON =
    Lude.withObject
      "SkillDetails"
      ( \x ->
          SkillDetails'
            Lude.<$> (x Lude..:? "SkillTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ProductDescription")
            Lude.<*> (x Lude..:? "InvocationPhrase")
            Lude.<*> (x Lude..:? "DeveloperInfo")
            Lude.<*> (x Lude..:? "EndUserLicenseAgreement")
            Lude.<*> (x Lude..:? "GenericKeywords" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Reviews" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ReleaseDate")
            Lude.<*> (x Lude..:? "NewInThisVersionBulletPoints" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "BulletPoints" Lude..!= Lude.mempty)
      )
