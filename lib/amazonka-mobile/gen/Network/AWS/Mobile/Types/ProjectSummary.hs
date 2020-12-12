{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectSummary
  ( ProjectSummary (..),

    -- * Smart constructor
    mkProjectSummary,

    -- * Lenses
    psName,
    psProjectId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summary information about an AWS Mobile Hub project.
--
-- /See:/ 'mkProjectSummary' smart constructor.
data ProjectSummary = ProjectSummary'
  { name :: Lude.Maybe Lude.Text,
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
-- * 'name' - Name of the project.
-- * 'projectId' - Unique project identifier.
mkProjectSummary ::
  ProjectSummary
mkProjectSummary =
  ProjectSummary' {name = Lude.Nothing, projectId = Lude.Nothing}

-- | Name of the project.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' ProjectSummary (Lude.Maybe Lude.Text)
psName = Lens.lens (name :: ProjectSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProjectSummary)
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Unique project identifier.
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
            Lude.<$> (x Lude..:? "name") Lude.<*> (x Lude..:? "projectId")
      )
