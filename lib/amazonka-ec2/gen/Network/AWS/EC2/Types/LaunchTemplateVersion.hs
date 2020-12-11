-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateVersion
  ( LaunchTemplateVersion (..),

    -- * Smart constructor
    mkLaunchTemplateVersion,

    -- * Lenses
    ltvLaunchTemplateName,
    ltvLaunchTemplateId,
    ltvCreatedBy,
    ltvDefaultVersion,
    ltvVersionNumber,
    ltvVersionDescription,
    ltvLaunchTemplateData,
    ltvCreateTime,
  )
where

import Network.AWS.EC2.Types.ResponseLaunchTemplateData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch template version.
--
-- /See:/ 'mkLaunchTemplateVersion' smart constructor.
data LaunchTemplateVersion = LaunchTemplateVersion'
  { launchTemplateName ::
      Lude.Maybe Lude.Text,
    launchTemplateId :: Lude.Maybe Lude.Text,
    createdBy :: Lude.Maybe Lude.Text,
    defaultVersion :: Lude.Maybe Lude.Bool,
    versionNumber :: Lude.Maybe Lude.Integer,
    versionDescription :: Lude.Maybe Lude.Text,
    launchTemplateData ::
      Lude.Maybe ResponseLaunchTemplateData,
    createTime :: Lude.Maybe Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateVersion' with the minimum fields required to make a request.
--
-- * 'createTime' - The time the version was created.
-- * 'createdBy' - The principal that created the version.
-- * 'defaultVersion' - Indicates whether the version is the default version.
-- * 'launchTemplateData' - Information about the launch template.
-- * 'launchTemplateId' - The ID of the launch template.
-- * 'launchTemplateName' - The name of the launch template.
-- * 'versionDescription' - The description for the version.
-- * 'versionNumber' - The version number.
mkLaunchTemplateVersion ::
  LaunchTemplateVersion
mkLaunchTemplateVersion =
  LaunchTemplateVersion'
    { launchTemplateName = Lude.Nothing,
      launchTemplateId = Lude.Nothing,
      createdBy = Lude.Nothing,
      defaultVersion = Lude.Nothing,
      versionNumber = Lude.Nothing,
      versionDescription = Lude.Nothing,
      launchTemplateData = Lude.Nothing,
      createTime = Lude.Nothing
    }

-- | The name of the launch template.
--
-- /Note:/ Consider using 'launchTemplateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateName :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.Text)
ltvLaunchTemplateName = Lens.lens (launchTemplateName :: LaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateName = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvLaunchTemplateName "Use generic-lens or generic-optics with 'launchTemplateName' instead." #-}

-- | The ID of the launch template.
--
-- /Note:/ Consider using 'launchTemplateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateId :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.Text)
ltvLaunchTemplateId = Lens.lens (launchTemplateId :: LaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {launchTemplateId = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvLaunchTemplateId "Use generic-lens or generic-optics with 'launchTemplateId' instead." #-}

-- | The principal that created the version.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvCreatedBy :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.Text)
ltvCreatedBy = Lens.lens (createdBy :: LaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {createdBy = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | Indicates whether the version is the default version.
--
-- /Note:/ Consider using 'defaultVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvDefaultVersion :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.Bool)
ltvDefaultVersion = Lens.lens (defaultVersion :: LaunchTemplateVersion -> Lude.Maybe Lude.Bool) (\s a -> s {defaultVersion = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvDefaultVersion "Use generic-lens or generic-optics with 'defaultVersion' instead." #-}

-- | The version number.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvVersionNumber :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.Integer)
ltvVersionNumber = Lens.lens (versionNumber :: LaunchTemplateVersion -> Lude.Maybe Lude.Integer) (\s a -> s {versionNumber = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The description for the version.
--
-- /Note:/ Consider using 'versionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvVersionDescription :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.Text)
ltvVersionDescription = Lens.lens (versionDescription :: LaunchTemplateVersion -> Lude.Maybe Lude.Text) (\s a -> s {versionDescription = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvVersionDescription "Use generic-lens or generic-optics with 'versionDescription' instead." #-}

-- | Information about the launch template.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvLaunchTemplateData :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe ResponseLaunchTemplateData)
ltvLaunchTemplateData = Lens.lens (launchTemplateData :: LaunchTemplateVersion -> Lude.Maybe ResponseLaunchTemplateData) (\s a -> s {launchTemplateData = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

-- | The time the version was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltvCreateTime :: Lens.Lens' LaunchTemplateVersion (Lude.Maybe Lude.ISO8601)
ltvCreateTime = Lens.lens (createTime :: LaunchTemplateVersion -> Lude.Maybe Lude.ISO8601) (\s a -> s {createTime = a} :: LaunchTemplateVersion)
{-# DEPRECATED ltvCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromXML LaunchTemplateVersion where
  parseXML x =
    LaunchTemplateVersion'
      Lude.<$> (x Lude..@? "launchTemplateName")
      Lude.<*> (x Lude..@? "launchTemplateId")
      Lude.<*> (x Lude..@? "createdBy")
      Lude.<*> (x Lude..@? "defaultVersion")
      Lude.<*> (x Lude..@? "versionNumber")
      Lude.<*> (x Lude..@? "versionDescription")
      Lude.<*> (x Lude..@? "launchTemplateData")
      Lude.<*> (x Lude..@? "createTime")
