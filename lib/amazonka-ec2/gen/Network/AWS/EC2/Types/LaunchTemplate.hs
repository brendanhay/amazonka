{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplate
  ( LaunchTemplate (..),

    -- * Smart constructor
    mkLaunchTemplate,

    -- * Lenses
    ltLaunchTemplateName,
    ltLatestVersionNumber,
    ltLaunchTemplateId,
    ltCreatedBy,
    ltDefaultVersionNumber,
    ltCreateTime,
    ltTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template.
--
-- /See:/ 'mkLaunchTemplate' smart constructor.
data LaunchTemplate = LaunchTemplate'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    latestVersionNumber :: Lude.Maybe Lude.Integer,
    launchTemplateId :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    defaultVersionNumber :: Lude.Maybe Lude.Integer,
    createTime :: Lude.Maybe Lude.DateTime,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplate' with the minimum fields required to make a request.
--
-- * 'createTime' - The time launch template was created.
-- * 'createdBy' - The principal that created the launch template.
-- * 'defaultVersionNumber' - The version number of the default version of the launch template.
-- * 'latestVersionNumber' - The version number of the latest version of the launch template.
-- * 'launchTemplateId' - The ID of the launch template.
-- * 'launchTemplateName' - The name of the launch template.
-- * 'tags' - The tags for the launch template.
mkLaunchTemplate ::
  LaunchTemplate
mkLaunchTemplate =
  LaunchTemplate'
    { launchTemplateName = Lude.Nothing,
      latestVersionNumber = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      createdBy = Lude.Nothing,
      defaultVersionNumber = Lude.Nothing,
      createTime = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateName :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.Text)
ltLaunchTemplateName = Lens.lens (launchTemplateName :: LaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: LaunchTemplate)
{-# DEPRECATED ltLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The version number of the latest version of the launch template.
--
-- /Note:/ Consider using 'latestVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLatestVersionNumber :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.Integer)
ltLatestVersionNumber = Lens.lens (latestVersionNumber :: LaunchTemplate -> Lude.Maybe Lude.Integer) (\s a -> s {latestVersionNumber = a} :: LaunchTemplate)
{-# DEPRECATED ltLatestVersionNumber "Use generic-lens or generic-optics with 'latestVersionNumber' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltLaunchTemplateId :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.Text)
ltLaunchTemplateId = Lens.lens (launchTemplateId :: LaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: LaunchTemplate)
{-# DEPRECATED ltLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The principal that created the launch template.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreatedBy :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.Text)
ltCreatedBy = Lens.lens (createdBy :: LaunchTemplate -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: LaunchTemplate)
{-# DEPRECATED ltCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The version number of the default version of the launch template.
--
-- /Note:/ Consider using 'defaultVersionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltDefaultVersionNumber :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.Integer)
ltDefaultVersionNumber = Lens.lens (defaultVersionNumber :: LaunchTemplate -> Lude.Maybe Lude.Integer) (\s a -> s {defaultVersionNumber = a} :: LaunchTemplate)
{-# DEPRECATED ltDefaultVersionNumber "Use generic-lens or generic-optics with 'defaultVersionNumber' instead." #-}

-- | The time launch template was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltCreateTime :: Lens.Lens' LaunchTemplate (Lude.Maybe Lude.DateTime)
ltCreateTime = Lens.lens (createTime :: LaunchTemplate -> Lude.Maybe Lude.DateTime) (\s a -> s {createTime = a} :: LaunchTemplate)
{-# DEPRECATED ltCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The tags for the launch template.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTags :: Lens.Lens' LaunchTemplate (Lude.Maybe [Tag])
ltTags = Lens.lens (tags :: LaunchTemplate -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: LaunchTemplate)
{-# DEPRECATED ltTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML LaunchTemplate where
  parseXML x =
    LaunchTemplate'
      Lude.<$> (x Lude..@? "launchTemplateName")
      Lude.<*> (x Lude..@? "latestVersionNumber")
      Lude.<*> (x Lude..@? "launchTemplateId")
      Lude.<*> (x Lude..@? "createdBy")
      Lude.<*> (x Lude..@? "defaultVersionNumber")
      Lude.<*> (x Lude..@? "createTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
