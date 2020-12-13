{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.VersionSummary
  ( VersionSummary (..),

    -- * Smart constructor
    mkVersionSummary,

    -- * Lenses
    vsCreationTime,
    vsSemanticVersion,
    vsSourceCodeURL,
    vsApplicationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An application version summary.
--
-- /See:/ 'mkVersionSummary' smart constructor.
data VersionSummary = VersionSummary'
  { -- | The date and time this resource was created.
    creationTime :: Lude.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Lude.Text,
    -- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
    sourceCodeURL :: Lude.Maybe Lude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VersionSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time this resource was created.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'sourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
-- * 'applicationId' - The application Amazon Resource Name (ARN).
mkVersionSummary ::
  -- | 'creationTime'
  Lude.Text ->
  -- | 'semanticVersion'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  VersionSummary
mkVersionSummary pCreationTime_ pSemanticVersion_ pApplicationId_ =
  VersionSummary'
    { creationTime = pCreationTime_,
      semanticVersion = pSemanticVersion_,
      sourceCodeURL = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsCreationTime :: Lens.Lens' VersionSummary Lude.Text
vsCreationTime = Lens.lens (creationTime :: VersionSummary -> Lude.Text) (\s a -> s {creationTime = a} :: VersionSummary)
{-# DEPRECATED vsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSemanticVersion :: Lens.Lens' VersionSummary Lude.Text
vsSemanticVersion = Lens.lens (semanticVersion :: VersionSummary -> Lude.Text) (\s a -> s {semanticVersion = a} :: VersionSummary)
{-# DEPRECATED vsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsSourceCodeURL :: Lens.Lens' VersionSummary (Lude.Maybe Lude.Text)
vsSourceCodeURL = Lens.lens (sourceCodeURL :: VersionSummary -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeURL = a} :: VersionSummary)
{-# DEPRECATED vsSourceCodeURL "Use generic-lens or generic-optics with 'sourceCodeURL' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsApplicationId :: Lens.Lens' VersionSummary Lude.Text
vsApplicationId = Lens.lens (applicationId :: VersionSummary -> Lude.Text) (\s a -> s {applicationId = a} :: VersionSummary)
{-# DEPRECATED vsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.FromJSON VersionSummary where
  parseJSON =
    Lude.withObject
      "VersionSummary"
      ( \x ->
          VersionSummary'
            Lude.<$> (x Lude..: "creationTime")
            Lude.<*> (x Lude..: "semanticVersion")
            Lude.<*> (x Lude..:? "sourceCodeUrl")
            Lude.<*> (x Lude..: "applicationId")
      )
