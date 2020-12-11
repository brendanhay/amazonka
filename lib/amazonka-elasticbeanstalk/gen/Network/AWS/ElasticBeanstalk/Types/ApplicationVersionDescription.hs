-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ApplicationVersionDescription
  ( ApplicationVersionDescription (..),

    -- * Smart constructor
    mkApplicationVersionDescription,

    -- * Lenses
    avdStatus,
    avdSourceBundle,
    avdDateUpdated,
    avdDateCreated,
    avdVersionLabel,
    avdSourceBuildInformation,
    avdApplicationName,
    avdApplicationVersionARN,
    avdBuildARN,
    avdDescription,
  )
where

import Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus
import Network.AWS.ElasticBeanstalk.Types.S3Location
import Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the properties of an application version.
--
-- /See:/ 'mkApplicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { status ::
      Lude.Maybe
        ApplicationVersionStatus,
    sourceBundle ::
      Lude.Maybe S3Location,
    dateUpdated ::
      Lude.Maybe Lude.ISO8601,
    dateCreated ::
      Lude.Maybe Lude.ISO8601,
    versionLabel ::
      Lude.Maybe Lude.Text,
    sourceBuildInformation ::
      Lude.Maybe
        SourceBuildInformation,
    applicationName ::
      Lude.Maybe Lude.Text,
    applicationVersionARN ::
      Lude.Maybe Lude.Text,
    buildARN ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationVersionDescription' with the minimum fields required to make a request.
--
-- * 'applicationName' - The name of the application to which the application version belongs.
-- * 'applicationVersionARN' - The Amazon Resource Name (ARN) of the application version.
-- * 'buildARN' - Reference to the artifact from the AWS CodeBuild build.
-- * 'dateCreated' - The creation date of the application version.
-- * 'dateUpdated' - The last modified date of the application version.
-- * 'description' - The description of the application version.
-- * 'sourceBuildInformation' - If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
-- * 'sourceBundle' - The storage location of the application version's source bundle in Amazon S3.
-- * 'status' - The processing status of the application version. Reflects the state of the application version during its creation. Many of the values are only applicable if you specified @True@ for the @Process@ parameter of the @CreateApplicationVersion@ action. The following list describes the possible values.
--
--
--     * @Unprocessed@ – Application version wasn't pre-processed or validated. Elastic Beanstalk will validate configuration files during deployment of the application version to an environment.
--
--
--     * @Processing@ – Elastic Beanstalk is currently processing the application version.
--
--
--     * @Building@ – Application version is currently undergoing an AWS CodeBuild build.
--
--
--     * @Processed@ – Elastic Beanstalk was successfully pre-processed and validated.
--
--
--     * @Failed@ – Either the AWS CodeBuild build failed or configuration files didn't pass validation. This application version isn't usable.
--
--
-- * 'versionLabel' - A unique identifier for the application version.
mkApplicationVersionDescription ::
  ApplicationVersionDescription
mkApplicationVersionDescription =
  ApplicationVersionDescription'
    { status = Lude.Nothing,
      sourceBundle = Lude.Nothing,
      dateUpdated = Lude.Nothing,
      dateCreated = Lude.Nothing,
      versionLabel = Lude.Nothing,
      sourceBuildInformation = Lude.Nothing,
      applicationName = Lude.Nothing,
      applicationVersionARN = Lude.Nothing,
      buildARN = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The processing status of the application version. Reflects the state of the application version during its creation. Many of the values are only applicable if you specified @True@ for the @Process@ parameter of the @CreateApplicationVersion@ action. The following list describes the possible values.
--
--
--     * @Unprocessed@ – Application version wasn't pre-processed or validated. Elastic Beanstalk will validate configuration files during deployment of the application version to an environment.
--
--
--     * @Processing@ – Elastic Beanstalk is currently processing the application version.
--
--
--     * @Building@ – Application version is currently undergoing an AWS CodeBuild build.
--
--
--     * @Processed@ – Elastic Beanstalk was successfully pre-processed and validated.
--
--
--     * @Failed@ – Either the AWS CodeBuild build failed or configuration files didn't pass validation. This application version isn't usable.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdStatus :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe ApplicationVersionStatus)
avdStatus = Lens.lens (status :: ApplicationVersionDescription -> Lude.Maybe ApplicationVersionStatus) (\s a -> s {status = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The storage location of the application version's source bundle in Amazon S3.
--
-- /Note:/ Consider using 'sourceBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdSourceBundle :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe S3Location)
avdSourceBundle = Lens.lens (sourceBundle :: ApplicationVersionDescription -> Lude.Maybe S3Location) (\s a -> s {sourceBundle = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdSourceBundle "Use generic-lens or generic-optics with 'sourceBundle' instead." #-}

-- | The last modified date of the application version.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdDateUpdated :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.ISO8601)
avdDateUpdated = Lens.lens (dateUpdated :: ApplicationVersionDescription -> Lude.Maybe Lude.ISO8601) (\s a -> s {dateUpdated = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | The creation date of the application version.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdDateCreated :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.ISO8601)
avdDateCreated = Lens.lens (dateCreated :: ApplicationVersionDescription -> Lude.Maybe Lude.ISO8601) (\s a -> s {dateCreated = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | A unique identifier for the application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdVersionLabel :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.Text)
avdVersionLabel = Lens.lens (versionLabel :: ApplicationVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {versionLabel = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

-- | If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
--
-- /Note:/ Consider using 'sourceBuildInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdSourceBuildInformation :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe SourceBuildInformation)
avdSourceBuildInformation = Lens.lens (sourceBuildInformation :: ApplicationVersionDescription -> Lude.Maybe SourceBuildInformation) (\s a -> s {sourceBuildInformation = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdSourceBuildInformation "Use generic-lens or generic-optics with 'sourceBuildInformation' instead." #-}

-- | The name of the application to which the application version belongs.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdApplicationName :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.Text)
avdApplicationName = Lens.lens (applicationName :: ApplicationVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationName = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The Amazon Resource Name (ARN) of the application version.
--
-- /Note:/ Consider using 'applicationVersionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdApplicationVersionARN :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.Text)
avdApplicationVersionARN = Lens.lens (applicationVersionARN :: ApplicationVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {applicationVersionARN = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdApplicationVersionARN "Use generic-lens or generic-optics with 'applicationVersionARN' instead." #-}

-- | Reference to the artifact from the AWS CodeBuild build.
--
-- /Note:/ Consider using 'buildARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdBuildARN :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.Text)
avdBuildARN = Lens.lens (buildARN :: ApplicationVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {buildARN = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdBuildARN "Use generic-lens or generic-optics with 'buildARN' instead." #-}

-- | The description of the application version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdDescription :: Lens.Lens' ApplicationVersionDescription (Lude.Maybe Lude.Text)
avdDescription = Lens.lens (description :: ApplicationVersionDescription -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ApplicationVersionDescription)
{-# DEPRECATED avdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML ApplicationVersionDescription where
  parseXML x =
    ApplicationVersionDescription'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "SourceBundle")
      Lude.<*> (x Lude..@? "DateUpdated")
      Lude.<*> (x Lude..@? "DateCreated")
      Lude.<*> (x Lude..@? "VersionLabel")
      Lude.<*> (x Lude..@? "SourceBuildInformation")
      Lude.<*> (x Lude..@? "ApplicationName")
      Lude.<*> (x Lude..@? "ApplicationVersionArn")
      Lude.<*> (x Lude..@? "BuildArn")
      Lude.<*> (x Lude..@? "Description")
