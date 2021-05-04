{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.SkillDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.SkillDetails where

import Network.AWS.AlexaBusiness.Types.DeveloperInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Granular information about the skill.
--
-- /See:/ 'newSkillDetails' smart constructor.
data SkillDetails = SkillDetails'
  { -- | The updates added in bullet points.
    newInThisVersionBulletPoints' :: Prelude.Maybe [Prelude.Text],
    -- | The types of skills.
    skillTypes :: Prelude.Maybe [Prelude.Text],
    -- | /This member has been deprecated./
    --
    -- The list of reviews for the skill, including Key and Value pair.
    reviews :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The details about what the skill supports organized as bullet points.
    bulletPoints :: Prelude.Maybe [Prelude.Text],
    -- | The generic keywords associated with the skill that can be used to find
    -- a skill.
    genericKeywords :: Prelude.Maybe [Prelude.Text],
    -- | The URL of the end user license agreement.
    endUserLicenseAgreement :: Prelude.Maybe Prelude.Text,
    -- | The details about the developer that published the skill.
    developerInfo :: Prelude.Maybe DeveloperInfo,
    -- | The description of the product.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The phrase used to trigger the skill.
    invocationPhrase :: Prelude.Maybe Prelude.Text,
    -- | The date when the skill was released.
    releaseDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SkillDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newInThisVersionBulletPoints'', 'skillDetails_newInThisVersionBulletPoints' - The updates added in bullet points.
--
-- 'skillTypes', 'skillDetails_skillTypes' - The types of skills.
--
-- 'reviews', 'skillDetails_reviews' - /This member has been deprecated./
--
-- The list of reviews for the skill, including Key and Value pair.
--
-- 'bulletPoints', 'skillDetails_bulletPoints' - The details about what the skill supports organized as bullet points.
--
-- 'genericKeywords', 'skillDetails_genericKeywords' - The generic keywords associated with the skill that can be used to find
-- a skill.
--
-- 'endUserLicenseAgreement', 'skillDetails_endUserLicenseAgreement' - The URL of the end user license agreement.
--
-- 'developerInfo', 'skillDetails_developerInfo' - The details about the developer that published the skill.
--
-- 'productDescription', 'skillDetails_productDescription' - The description of the product.
--
-- 'invocationPhrase', 'skillDetails_invocationPhrase' - The phrase used to trigger the skill.
--
-- 'releaseDate', 'skillDetails_releaseDate' - The date when the skill was released.
newSkillDetails ::
  SkillDetails
newSkillDetails =
  SkillDetails'
    { newInThisVersionBulletPoints' =
        Prelude.Nothing,
      skillTypes = Prelude.Nothing,
      reviews = Prelude.Nothing,
      bulletPoints = Prelude.Nothing,
      genericKeywords = Prelude.Nothing,
      endUserLicenseAgreement = Prelude.Nothing,
      developerInfo = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      invocationPhrase = Prelude.Nothing,
      releaseDate = Prelude.Nothing
    }

-- | The updates added in bullet points.
skillDetails_newInThisVersionBulletPoints :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_newInThisVersionBulletPoints = Lens.lens (\SkillDetails' {newInThisVersionBulletPoints'} -> newInThisVersionBulletPoints') (\s@SkillDetails' {} a -> s {newInThisVersionBulletPoints' = a} :: SkillDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The types of skills.
skillDetails_skillTypes :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_skillTypes = Lens.lens (\SkillDetails' {skillTypes} -> skillTypes) (\s@SkillDetails' {} a -> s {skillTypes = a} :: SkillDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | /This member has been deprecated./
--
-- The list of reviews for the skill, including Key and Value pair.
skillDetails_reviews :: Lens.Lens' SkillDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
skillDetails_reviews = Lens.lens (\SkillDetails' {reviews} -> reviews) (\s@SkillDetails' {} a -> s {reviews = a} :: SkillDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The details about what the skill supports organized as bullet points.
skillDetails_bulletPoints :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_bulletPoints = Lens.lens (\SkillDetails' {bulletPoints} -> bulletPoints) (\s@SkillDetails' {} a -> s {bulletPoints = a} :: SkillDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The generic keywords associated with the skill that can be used to find
-- a skill.
skillDetails_genericKeywords :: Lens.Lens' SkillDetails (Prelude.Maybe [Prelude.Text])
skillDetails_genericKeywords = Lens.lens (\SkillDetails' {genericKeywords} -> genericKeywords) (\s@SkillDetails' {} a -> s {genericKeywords = a} :: SkillDetails) Prelude.. Lens.mapping Prelude._Coerce

-- | The URL of the end user license agreement.
skillDetails_endUserLicenseAgreement :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_endUserLicenseAgreement = Lens.lens (\SkillDetails' {endUserLicenseAgreement} -> endUserLicenseAgreement) (\s@SkillDetails' {} a -> s {endUserLicenseAgreement = a} :: SkillDetails)

-- | The details about the developer that published the skill.
skillDetails_developerInfo :: Lens.Lens' SkillDetails (Prelude.Maybe DeveloperInfo)
skillDetails_developerInfo = Lens.lens (\SkillDetails' {developerInfo} -> developerInfo) (\s@SkillDetails' {} a -> s {developerInfo = a} :: SkillDetails)

-- | The description of the product.
skillDetails_productDescription :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_productDescription = Lens.lens (\SkillDetails' {productDescription} -> productDescription) (\s@SkillDetails' {} a -> s {productDescription = a} :: SkillDetails)

-- | The phrase used to trigger the skill.
skillDetails_invocationPhrase :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_invocationPhrase = Lens.lens (\SkillDetails' {invocationPhrase} -> invocationPhrase) (\s@SkillDetails' {} a -> s {invocationPhrase = a} :: SkillDetails)

-- | The date when the skill was released.
skillDetails_releaseDate :: Lens.Lens' SkillDetails (Prelude.Maybe Prelude.Text)
skillDetails_releaseDate = Lens.lens (\SkillDetails' {releaseDate} -> releaseDate) (\s@SkillDetails' {} a -> s {releaseDate = a} :: SkillDetails)

instance Prelude.FromJSON SkillDetails where
  parseJSON =
    Prelude.withObject
      "SkillDetails"
      ( \x ->
          SkillDetails'
            Prelude.<$> ( x Prelude..:? "NewInThisVersionBulletPoints"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "SkillTypes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Reviews" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "BulletPoints"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "GenericKeywords"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "EndUserLicenseAgreement")
            Prelude.<*> (x Prelude..:? "DeveloperInfo")
            Prelude.<*> (x Prelude..:? "ProductDescription")
            Prelude.<*> (x Prelude..:? "InvocationPhrase")
            Prelude.<*> (x Prelude..:? "ReleaseDate")
      )

instance Prelude.Hashable SkillDetails

instance Prelude.NFData SkillDetails
