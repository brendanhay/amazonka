-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteLaunchTemplateVersionsResponseSuccessItem
  ( DeleteLaunchTemplateVersionsResponseSuccessItem (..),

    -- * Smart constructor
    mkDeleteLaunchTemplateVersionsResponseSuccessItem,

    -- * Lenses
    dltvrsiLaunchTemplateName,
    dltvrsiLaunchTemplateId,
    dltvrsiVersionNumber,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template version that was successfully deleted.
--
-- /See:/ 'mkDeleteLaunchTemplateVersionsResponseSuccessItem' smart constructor.
data DeleteLaunchTemplateVersionsResponseSuccessItem = DeleteLaunchTemplateVersionsResponseSuccessItem'
  { launchTemplateName ::
      Lude.Maybe
        Lude.Text,
    launchTemplateId ::
      Lude.Maybe
        Lude.Text,
    versionNumber ::
      Lude.Maybe
        Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DeleteLaunchTemplateVersionsResponseSuccessItem' with the minimum fields required to make a request.
--
-- * 'launchTemplateId' - The ID of the launch template.
-- * 'launchTemplateName' - The name of the launch template.
-- * 'versionNumber' - The version number of the launch template.
mkDeleteLaunchTemplateVersionsResponseSuccessItem ::
  DeleteLaunchTemplateVersionsResponseSuccessItem
mkDeleteLaunchTemplateVersionsResponseSuccessItem =
  DeleteLaunchTemplateVersionsResponseSuccessItem'
    { launchTemplateName =
        Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      versionNumber = Lude.Nothing
    }

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiLaunchTemplateName :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Lude.Maybe Lude.Text)
dltvrsiLaunchTemplateName = Lens.lens (launchTemplateName :: DeleteLaunchTemplateVersionsResponseSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: DeleteLaunchTemplateVersionsResponseSuccessItem)
{-# DEPRECATED dltvrsiLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiLaunchTemplateId :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Lude.Maybe Lude.Text)
dltvrsiLaunchTemplateId = Lens.lens (launchTemplateId :: DeleteLaunchTemplateVersionsResponseSuccessItem -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: DeleteLaunchTemplateVersionsResponseSuccessItem)
{-# DEPRECATED dltvrsiLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The version number of the launch template.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dltvrsiVersionNumber :: Lens.Lens' DeleteLaunchTemplateVersionsResponseSuccessItem (Lude.Maybe Lude.Integer)
dltvrsiVersionNumber = Lens.lens (versionNumber :: DeleteLaunchTemplateVersionsResponseSuccessItem -> Lude.Maybe Lude.Integer) (\s a -> s {versionNumber = a} :: DeleteLaunchTemplateVersionsResponseSuccessItem)
{-# DEPRECATED dltvrsiVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

instance
  Lude.FromXML
    DeleteLaunchTemplateVersionsResponseSuccessItem
  where
  parseXML x =
    DeleteLaunchTemplateVersionsResponseSuccessItem'
      Lude.<$> (x Lude..@? "launchTemplateName")
      Lude.<*> (x Lude..@? "launchTemplateId")
      Lude.<*> (x Lude..@? "versionNumber")
