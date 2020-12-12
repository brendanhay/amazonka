{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationDescription
  ( ApplicationDescription (..),

    -- * Smart constructor
    mkApplicationDescription,

    -- * Lenses
    adApplicationARN,
    adVersions,
    adDateUpdated,
    adDateCreated,
    adApplicationName,
    adConfigurationTemplates,
    adResourceLifecycleConfig,
    adDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ApplicationResourceLifecycleConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of an application.
--
-- /See:/ 'mkApplicationDescription' smart constructor.
data ApplicationDescription = ApplicationDescription'
  { applicationARN ::
      Lude.Maybe Lude.Text,
    versions :: Lude.Maybe [Lude.Text],
    dateUpdated :: Lude.Maybe Lude.DateTime,
    dateCreated :: Lude.Maybe Lude.DateTime,
    applicationName :: Lude.Maybe Lude.Text,
    configurationTemplates ::
      Lude.Maybe [Lude.Text],
    resourceLifecycleConfig ::
      Lude.Maybe ApplicationResourceLifecycleConfig,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationDescription' with the minimum fields required to make a request.
--
-- * 'applicationARN' - The Amazon Resource Name (ARN) of the application.
-- * 'applicationName' - The name of the application.
-- * 'configurationTemplates' - The names of the configuration templates associated with this application.
-- * 'dateCreated' - The date when the application was created.
-- * 'dateUpdated' - The date when the application was last modified.
-- * 'description' - User-defined description of the application.
-- * 'resourceLifecycleConfig' - The lifecycle settings for the application.
-- * 'versions' - The names of the versions for this application.
mkApplicationDescription ::
  ApplicationDescription
mkApplicationDescription =
  ApplicationDescription'
    { applicationARN = Lude.Nothing,
      versions = Lude.Nothing,
      dateUpdated = Lude.Nothing,
      dateCreated = Lude.Nothing,
      applicationName = Lude.Nothing,
      configurationTemplates = Lude.Nothing,
      resourceLifecycleConfig = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationARN :: Lens.Lens' ApplicationDescription (Lude.Maybe Lude.Text)
adApplicationARN = Lens.lens (applicationARN :: ApplicationDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationARN = a} :: ApplicationDescription)
{-# DEPRECATED adApplicationARN "Use generic-lens or generic-optics with 'applicationARN' instead." #-}

-- | The names of the versions for this application.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adVersions :: Lens.Lens' ApplicationDescription (Lude.Maybe [Lude.Text])
adVersions = Lens.lens (versions :: ApplicationDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {versions = a} :: ApplicationDescription)
{-# DEPRECATED adVersions "Use generic-lens or generic-optics with 'versions' instead." #-}

-- | The date when the application was last modified.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDateUpdated :: Lens.Lens' ApplicationDescription (Lude.Maybe Lude.DateTime)
adDateUpdated = Lens.lens (dateUpdated :: ApplicationDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateUpdated = a} :: ApplicationDescription)
{-# DEPRECATED adDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | The date when the application was created.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDateCreated :: Lens.Lens' ApplicationDescription (Lude.Maybe Lude.DateTime)
adDateCreated = Lens.lens (dateCreated :: ApplicationDescription -> Lude.Maybe Lude.DateTime) (\s a -> s {dateCreated = a} :: ApplicationDescription)
{-# DEPRECATED adDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adApplicationName :: Lens.Lens' ApplicationDescription (Lude.Maybe Lude.Text)
adApplicationName = Lens.lens (applicationName :: ApplicationDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ApplicationDescription)
{-# DEPRECATED adApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The names of the configuration templates associated with this application.
--
-- /Note:/ Consider using 'configurationTemplates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adConfigurationTemplates :: Lens.Lens' ApplicationDescription (Lude.Maybe [Lude.Text])
adConfigurationTemplates = Lens.lens (configurationTemplates :: ApplicationDescription -> Lude.Maybe [Lude.Text]) (\s a -> s {configurationTemplates = a} :: ApplicationDescription)
{-# DEPRECATED adConfigurationTemplates "Use generic-lens or generic-optics with 'configurationTemplates' instead." #-}

-- | The lifecycle settings for the application.
--
-- /Note:/ Consider using 'resourceLifecycleConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adResourceLifecycleConfig :: Lens.Lens' ApplicationDescription (Lude.Maybe ApplicationResourceLifecycleConfig)
adResourceLifecycleConfig = Lens.lens (resourceLifecycleConfig :: ApplicationDescription -> Lude.Maybe ApplicationResourceLifecycleConfig) (\s a -> s {resourceLifecycleConfig = a} :: ApplicationDescription)
{-# DEPRECATED adResourceLifecycleConfig "Use generic-lens or generic-optics with 'resourceLifecycleConfig' instead." #-}

-- | User-defined description of the application.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDescription :: Lens.Lens' ApplicationDescription (Lude.Maybe Lude.Text)
adDescription = Lens.lens (description :: ApplicationDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ApplicationDescription)
{-# DEPRECATED adDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ApplicationDescription where
  parseXML x =
    ApplicationDescription'
      Lude.<$> (x Lude..@? "ApplicationArn")
      Lude.<*> ( x Lude..@? "Versions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "DateUpdated")
      Lude.<*> (x Lude..@? "DateCreated")
      Lude.<*> (x Lude..@? "ApplicationName")
      Lude.<*> ( x Lude..@? "ConfigurationTemplates" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ResourceLifecycleConfig")
      Lude.<*> (x Lude..@? "Description")
