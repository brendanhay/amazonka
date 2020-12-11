-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ProjectDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ProjectDescription
  ( ProjectDescription (..),

    -- * Smart constructor
    mkProjectDescription,

    -- * Lenses
    pdStatus,
    pdCreationTimestamp,
    pdProjectARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.ProjectStatus

-- | A description of a Amazon Rekognition Custom Labels project.
--
-- /See:/ 'mkProjectDescription' smart constructor.
data ProjectDescription = ProjectDescription'
  { status ::
      Lude.Maybe ProjectStatus,
    creationTimestamp :: Lude.Maybe Lude.Timestamp,
    projectARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectDescription' with the minimum fields required to make a request.
--
-- * 'creationTimestamp' - The Unix timestamp for the date and time that the project was created.
-- * 'projectARN' - The Amazon Resource Name (ARN) of the project.
-- * 'status' - The current status of the project.
mkProjectDescription ::
  ProjectDescription
mkProjectDescription =
  ProjectDescription'
    { status = Lude.Nothing,
      creationTimestamp = Lude.Nothing,
      projectARN = Lude.Nothing
    }

-- | The current status of the project.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdStatus :: Lens.Lens' ProjectDescription (Lude.Maybe ProjectStatus)
pdStatus = Lens.lens (status :: ProjectDescription -> Lude.Maybe ProjectStatus) (\s a -> s {status = a} :: ProjectDescription)
{-# DEPRECATED pdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The Unix timestamp for the date and time that the project was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreationTimestamp :: Lens.Lens' ProjectDescription (Lude.Maybe Lude.Timestamp)
pdCreationTimestamp = Lens.lens (creationTimestamp :: ProjectDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTimestamp = a} :: ProjectDescription)
{-# DEPRECATED pdCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The Amazon Resource Name (ARN) of the project.
--
-- /Note:/ Consider using 'projectARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProjectARN :: Lens.Lens' ProjectDescription (Lude.Maybe Lude.Text)
pdProjectARN = Lens.lens (projectARN :: ProjectDescription -> Lude.Maybe Lude.Text) (\s a -> s {projectARN = a} :: ProjectDescription)
{-# DEPRECATED pdProjectARN "Use generic-lens or generic-optics with 'projectARN' instead." #-}

instance Lude.FromJSON ProjectDescription where
  parseJSON =
    Lude.withObject
      "ProjectDescription"
      ( \x ->
          ProjectDescription'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "CreationTimestamp")
            Lude.<*> (x Lude..:? "ProjectArn")
      )
