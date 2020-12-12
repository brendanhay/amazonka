{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types.ProjectDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types.ProjectDetails
  ( ProjectDetails (..),

    -- * Smart constructor
    mkProjectDetails,

    -- * Lenses
    pdState,
    pdResources,
    pdCreatedDate,
    pdConsoleURL,
    pdName,
    pdRegion,
    pdProjectId,
    pdLastUpdatedDate,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.Resource
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about an AWS Mobile Hub project.
--
-- /See:/ 'mkProjectDetails' smart constructor.
data ProjectDetails = ProjectDetails'
  { state ::
      Lude.Maybe ProjectState,
    resources :: Lude.Maybe [Resource],
    createdDate :: Lude.Maybe Lude.Timestamp,
    consoleURL :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text,
    projectId :: Lude.Maybe Lude.Text,
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ProjectDetails' with the minimum fields required to make a request.
--
-- * 'consoleURL' - Website URL for this project in the AWS Mobile Hub console.
-- * 'createdDate' - Date the project was created.
-- * 'lastUpdatedDate' - Date of the last modification of the project.
-- * 'name' - Undocumented field.
-- * 'projectId' - Undocumented field.
-- * 'region' - Undocumented field.
-- * 'resources' - Undocumented field.
-- * 'state' - Undocumented field.
mkProjectDetails ::
  ProjectDetails
mkProjectDetails =
  ProjectDetails'
    { state = Lude.Nothing,
      resources = Lude.Nothing,
      createdDate = Lude.Nothing,
      consoleURL = Lude.Nothing,
      name = Lude.Nothing,
      region = Lude.Nothing,
      projectId = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdState :: Lens.Lens' ProjectDetails (Lude.Maybe ProjectState)
pdState = Lens.lens (state :: ProjectDetails -> Lude.Maybe ProjectState) (\s a -> s {state = a} :: ProjectDetails)
{-# DEPRECATED pdState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdResources :: Lens.Lens' ProjectDetails (Lude.Maybe [Resource])
pdResources = Lens.lens (resources :: ProjectDetails -> Lude.Maybe [Resource]) (\s a -> s {resources = a} :: ProjectDetails)
{-# DEPRECATED pdResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | Date the project was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCreatedDate :: Lens.Lens' ProjectDetails (Lude.Maybe Lude.Timestamp)
pdCreatedDate = Lens.lens (createdDate :: ProjectDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: ProjectDetails)
{-# DEPRECATED pdCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | Website URL for this project in the AWS Mobile Hub console.
--
-- /Note:/ Consider using 'consoleURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdConsoleURL :: Lens.Lens' ProjectDetails (Lude.Maybe Lude.Text)
pdConsoleURL = Lens.lens (consoleURL :: ProjectDetails -> Lude.Maybe Lude.Text) (\s a -> s {consoleURL = a} :: ProjectDetails)
{-# DEPRECATED pdConsoleURL "Use generic-lens or generic-optics with 'consoleURL' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdName :: Lens.Lens' ProjectDetails (Lude.Maybe Lude.Text)
pdName = Lens.lens (name :: ProjectDetails -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ProjectDetails)
{-# DEPRECATED pdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdRegion :: Lens.Lens' ProjectDetails (Lude.Maybe Lude.Text)
pdRegion = Lens.lens (region :: ProjectDetails -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ProjectDetails)
{-# DEPRECATED pdRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'projectId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdProjectId :: Lens.Lens' ProjectDetails (Lude.Maybe Lude.Text)
pdProjectId = Lens.lens (projectId :: ProjectDetails -> Lude.Maybe Lude.Text) (\s a -> s {projectId = a} :: ProjectDetails)
{-# DEPRECATED pdProjectId "Use generic-lens or generic-optics with 'projectId' instead." #-}

-- | Date of the last modification of the project.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdLastUpdatedDate :: Lens.Lens' ProjectDetails (Lude.Maybe Lude.Timestamp)
pdLastUpdatedDate = Lens.lens (lastUpdatedDate :: ProjectDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: ProjectDetails)
{-# DEPRECATED pdLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

instance Lude.FromJSON ProjectDetails where
  parseJSON =
    Lude.withObject
      "ProjectDetails"
      ( \x ->
          ProjectDetails'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resources" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "consoleUrl")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "projectId")
            Lude.<*> (x Lude..:? "lastUpdatedDate")
      )
