{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.ProjectSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.ProjectSummary
  ( ProjectSummary (..),

    -- * Smart constructor
    mkProjectSummary,

    -- * Lenses
    psProjectARN,
    psProjectId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the metadata for a project.
--
-- /See:/ 'mkProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { projectARN ::
      Lude.Maybe Lude.Text,
    projectId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectSummary' with the minimum fields required to make a request.
--
-- * 'projectARN' - The Amazon Resource Name (ARN) of the project.
-- * 'projectId' - The ID of the project.
mkProjectSummary ::
  ProjectSummary
mkProjectSummary =
  ProjectSummary'
    { projectARN = Lude.Nothing,
      projectId = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psProjectARN :: Lens.Lens' ProjectSummary (Lude.Maybe Lude.Text)
psProjectARN = Lens.lens (projectARN :: ProjectSummary -> Lude.Maybe Lude.Text) (\s a -> s {projectARN = a} :: ProjectSummary)
{-# DEPRECATED psProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

-- | The ID of the project.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psProjectId :: Lens.Lens' ProjectSummary (Lude.Maybe Lude.Text)
psProjectId = Lens.lens (projectId :: ProjectSummary -> Lude.Maybe Lude.Text) (\s a -> s {projectId = a} :: ProjectSummary)
{-# DEPRECATED psProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

instance Lude.FromJSON ProjectSummary where
  parseJSON =
    Lude.withObject
      "ProjectSummary"
      ( \x ->
          ProjectSummary'
            Lude.<$> (x Lude..:? "projectArn") Lude.<*> (x Lude..:? "projectId")
      )
