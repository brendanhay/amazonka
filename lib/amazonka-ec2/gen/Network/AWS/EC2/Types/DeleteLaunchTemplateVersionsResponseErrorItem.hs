{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseErrorItem
  ( DeleteLaunchTemplateVersionsResponseErrorItem (..),

    -- * Smart constructor
    mkDeleteLaunchTemplateVersionsResponseErrorItem,

    -- * Lenses
    dltvreiLaunchTemplateName,
    dltvreiLaunchTemplateId,
    dltvreiVersionNumber,
    dltvreiResponseError,
  )
where

import Network.AWS.EC2.Types.ResponseError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template version that could not be deleted.
--
-- /See:/ 'mkDeleteLaunchTemplateVersionsResponseErrorItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseErrorItem = DeleteLaunchTemplateVersionsResponseErrorItem'
  { -- | The name of the launch template.
    launchTemplateName :: Lude.Maybe Lude.Text,
    -- | The ID of the launch template.
    launchTemplateId :: Lude.Maybe Lude.Text,
    -- | The version number of the launch template.
    versionNumber :: Lude.Maybe Lude.Integer,
    -- | Information about the error.
    responseError :: Lude.Maybe ResponseError
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLaunchTemplateVersionsResponseErrorItem' with the minimum fields required to make a request.
--
-- * 'launchTemplateName' - The name of the launch template.
-- * 'launchTemplateId' - The ID of the launch template.
-- * 'versionNumber' - The version number of the launch template.
-- * 'responseError' - Information about the error.
mkDeleteLaunchTemplateVersionsResponseErrorItem ::
  DeleteLaunchTemplateVersionsResponseErrorItem
mkDeleteLaunchTemplateVersionsResponseErrorItem =
  DeleteLaunchTemplateVersionsResponseErrorItem'
    { launchTemplateName =
        Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      versionNumber = Lude.Nothing,
      responseError = Lude.Nothing
    }

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Lude.Maybe Lude.Text)
dltvreiLaunchTemplateName = Lens.lens (launchTemplateName :: DeleteLaunchTemplateVersionsResponseErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)
{-# DEPRECATED dltvreiLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Lude.Maybe Lude.Text)
dltvreiLaunchTemplateId = Lens.lens (launchTemplateId :: DeleteLaunchTemplateVersionsResponseErrorItem -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)
{-# DEPRECATED dltvreiLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version number of the launch template.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiVersionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Lude.Maybe Lude.Integer)
dltvreiVersionNumber = Lens.lens (versionNumber :: DeleteLaunchTemplateVersionsResponseErrorItem -> Lude.Maybe Lude.Integer) (\s a -> s {versionNumber = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)
{-# DEPRECATED dltvreiVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | Information about the error.
--
-- /Note:/ Consider using 'responseError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvreiResponseError :: Lens.Lens' DeleteLaunchTemplateVersionsResponseErrorItem (Lude.Maybe ResponseError)
dltvreiResponseError = Lens.lens (responseError :: DeleteLaunchTemplateVersionsResponseErrorItem -> Lude.Maybe ResponseError) (\s a -> s {responseError = a} :: DeleteLaunchTemplateVersionsResponseErrorItem)
{-# DEPRECATED dltvreiResponseError "Use generic-lens or generic-optics with 'responseError' instead." #-}

instance Lude.FromXML DeleteLaunchTemplateVersionsResponseErrorItem where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseErrorItem'
      Lude.<$> (x Lude..@? "launchTemplateName")
      Lude.<*> (x Lude..@? "launchTemplateId")
      Lude.<*> (x Lude..@? "versionNumber")
      Lude.<*> (x Lude..@? "responseError")
