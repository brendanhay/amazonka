-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
  ( ApplicationSummary (..),

    -- * Smart constructor
    mkApplicationSummary,

    -- * Lenses
    asCreationTime,
    asHomePageURL,
    asLabels,
    asSpdxLicenseId,
    asDescription,
    asAuthor,
    asApplicationId,
    asName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary of details about the application.
--
-- /See:/ 'mkApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { creationTime ::
      Lude.Maybe Lude.Text,
    homePageURL :: Lude.Maybe Lude.Text,
    labels :: Lude.Maybe [Lude.Text],
    spdxLicenseId :: Lude.Maybe Lude.Text,
    description :: Lude.Text,
    author :: Lude.Text,
    applicationId :: Lude.Text,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationSummary' with the minimum fields required to make a request.
--
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
-- * 'creationTime' - The date and time this resource was created.
-- * 'description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
-- * 'homePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
-- * 'labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
-- * 'name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
-- * 'spdxLicenseId' - A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
mkApplicationSummary ::
  -- | 'description'
  Lude.Text ->
  -- | 'author'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ApplicationSummary
mkApplicationSummary pDescription_ pAuthor_ pApplicationId_ pName_ =
  ApplicationSummary'
    { creationTime = Lude.Nothing,
      homePageURL = Lude.Nothing,
      labels = Lude.Nothing,
      spdxLicenseId = Lude.Nothing,
      description = pDescription_,
      author = pAuthor_,
      applicationId = pApplicationId_,
      name = pName_
    }

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCreationTime :: Lens.Lens' ApplicationSummary (Lude.Maybe Lude.Text)
asCreationTime = Lens.lens (creationTime :: ApplicationSummary -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: ApplicationSummary)
{-# DEPRECATED asCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asHomePageURL :: Lens.Lens' ApplicationSummary (Lude.Maybe Lude.Text)
asHomePageURL = Lens.lens (homePageURL :: ApplicationSummary -> Lude.Maybe Lude.Text) (\s a -> s {homePageURL = a} :: ApplicationSummary)
{-# DEPRECATED asHomePageURL "Use generic-lens or generic-optics with 'homePageURL' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLabels :: Lens.Lens' ApplicationSummary (Lude.Maybe [Lude.Text])
asLabels = Lens.lens (labels :: ApplicationSummary -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: ApplicationSummary)
{-# DEPRECATED asLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSpdxLicenseId :: Lens.Lens' ApplicationSummary (Lude.Maybe Lude.Text)
asSpdxLicenseId = Lens.lens (spdxLicenseId :: ApplicationSummary -> Lude.Maybe Lude.Text) (\s a -> s {spdxLicenseId = a} :: ApplicationSummary)
{-# DEPRECATED asSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDescription :: Lens.Lens' ApplicationSummary Lude.Text
asDescription = Lens.lens (description :: ApplicationSummary -> Lude.Text) (\s a -> s {description = a} :: ApplicationSummary)
{-# DEPRECATED asDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAuthor :: Lens.Lens' ApplicationSummary Lude.Text
asAuthor = Lens.lens (author :: ApplicationSummary -> Lude.Text) (\s a -> s {author = a} :: ApplicationSummary)
{-# DEPRECATED asAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationId :: Lens.Lens' ApplicationSummary Lude.Text
asApplicationId = Lens.lens (applicationId :: ApplicationSummary -> Lude.Text) (\s a -> s {applicationId = a} :: ApplicationSummary)
{-# DEPRECATED asApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' ApplicationSummary Lude.Text
asName = Lens.lens (name :: ApplicationSummary -> Lude.Text) (\s a -> s {name = a} :: ApplicationSummary)
{-# DEPRECATED asName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON ApplicationSummary where
  parseJSON =
    Lude.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Lude.<$> (x Lude..:? "creationTime")
            Lude.<*> (x Lude..:? "homePageUrl")
            Lude.<*> (x Lude..:? "labels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "spdxLicenseId")
            Lude.<*> (x Lude..: "description")
            Lude.<*> (x Lude..: "author")
            Lude.<*> (x Lude..: "applicationId")
            Lude.<*> (x Lude..: "name")
      )
