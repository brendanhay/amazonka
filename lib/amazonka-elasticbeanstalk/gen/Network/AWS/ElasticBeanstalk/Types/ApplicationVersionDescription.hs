{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    avdApplicationName,
    avdApplicationVersionArn,
    avdBuildArn,
    avdDateCreated,
    avdDateUpdated,
    avdDescription,
    avdSourceBuildInformation,
    avdSourceBundle,
    avdStatus,
    avdVersionLabel,
  )
where

import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationName as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationVersionArn as Types
import qualified Network.AWS.ElasticBeanstalk.Types.ApplicationVersionStatus as Types
import qualified Network.AWS.ElasticBeanstalk.Types.Description as Types
import qualified Network.AWS.ElasticBeanstalk.Types.S3Location as Types
import qualified Network.AWS.ElasticBeanstalk.Types.SourceBuildInformation as Types
import qualified Network.AWS.ElasticBeanstalk.Types.String as Types
import qualified Network.AWS.ElasticBeanstalk.Types.VersionLabel as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the properties of an application version.
--
-- /See:/ 'mkApplicationVersionDescription' smart constructor.
data ApplicationVersionDescription = ApplicationVersionDescription'
  { -- | The name of the application to which the application version belongs.
    applicationName :: Core.Maybe Types.ApplicationName,
    -- | The Amazon Resource Name (ARN) of the application version.
    applicationVersionArn :: Core.Maybe Types.ApplicationVersionArn,
    -- | Reference to the artifact from the AWS CodeBuild build.
    buildArn :: Core.Maybe Types.String,
    -- | The creation date of the application version.
    dateCreated :: Core.Maybe Core.UTCTime,
    -- | The last modified date of the application version.
    dateUpdated :: Core.Maybe Core.UTCTime,
    -- | The description of the application version.
    description :: Core.Maybe Types.Description,
    -- | If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
    sourceBuildInformation :: Core.Maybe Types.SourceBuildInformation,
    -- | The storage location of the application version's source bundle in Amazon S3.
    sourceBundle :: Core.Maybe Types.S3Location,
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
    status :: Core.Maybe Types.ApplicationVersionStatus,
    -- | A unique identifier for the application version.
    versionLabel :: Core.Maybe Types.VersionLabel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ApplicationVersionDescription' value with any optional fields omitted.
mkApplicationVersionDescription ::
  ApplicationVersionDescription
mkApplicationVersionDescription =
  ApplicationVersionDescription'
    { applicationName = Core.Nothing,
      applicationVersionArn = Core.Nothing,
      buildArn = Core.Nothing,
      dateCreated = Core.Nothing,
      dateUpdated = Core.Nothing,
      description = Core.Nothing,
      sourceBuildInformation = Core.Nothing,
      sourceBundle = Core.Nothing,
      status = Core.Nothing,
      versionLabel = Core.Nothing
    }

-- | The name of the application to which the application version belongs.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdApplicationName :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.ApplicationName)
avdApplicationName = Lens.field @"applicationName"
{-# DEPRECATED avdApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The Amazon Resource Name (ARN) of the application version.
--
-- /Note:/ Consider using 'applicationVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdApplicationVersionArn :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.ApplicationVersionArn)
avdApplicationVersionArn = Lens.field @"applicationVersionArn"
{-# DEPRECATED avdApplicationVersionArn "Use generic-lens or generic-optics with 'applicationVersionArn' instead." #-}

-- | Reference to the artifact from the AWS CodeBuild build.
--
-- /Note:/ Consider using 'buildArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdBuildArn :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.String)
avdBuildArn = Lens.field @"buildArn"
{-# DEPRECATED avdBuildArn "Use generic-lens or generic-optics with 'buildArn' instead." #-}

-- | The creation date of the application version.
--
-- /Note:/ Consider using 'dateCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdDateCreated :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.UTCTime)
avdDateCreated = Lens.field @"dateCreated"
{-# DEPRECATED avdDateCreated "Use generic-lens or generic-optics with 'dateCreated' instead." #-}

-- | The last modified date of the application version.
--
-- /Note:/ Consider using 'dateUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdDateUpdated :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Core.UTCTime)
avdDateUpdated = Lens.field @"dateUpdated"
{-# DEPRECATED avdDateUpdated "Use generic-lens or generic-optics with 'dateUpdated' instead." #-}

-- | The description of the application version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdDescription :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.Description)
avdDescription = Lens.field @"description"
{-# DEPRECATED avdDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | If the version's source code was retrieved from AWS CodeCommit, the location of the source code for the application version.
--
-- /Note:/ Consider using 'sourceBuildInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdSourceBuildInformation :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.SourceBuildInformation)
avdSourceBuildInformation = Lens.field @"sourceBuildInformation"
{-# DEPRECATED avdSourceBuildInformation "Use generic-lens or generic-optics with 'sourceBuildInformation' instead." #-}

-- | The storage location of the application version's source bundle in Amazon S3.
--
-- /Note:/ Consider using 'sourceBundle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdSourceBundle :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.S3Location)
avdSourceBundle = Lens.field @"sourceBundle"
{-# DEPRECATED avdSourceBundle "Use generic-lens or generic-optics with 'sourceBundle' instead." #-}

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
avdStatus :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.ApplicationVersionStatus)
avdStatus = Lens.field @"status"
{-# DEPRECATED avdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique identifier for the application version.
--
-- /Note:/ Consider using 'versionLabel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdVersionLabel :: Lens.Lens' ApplicationVersionDescription (Core.Maybe Types.VersionLabel)
avdVersionLabel = Lens.field @"versionLabel"
{-# DEPRECATED avdVersionLabel "Use generic-lens or generic-optics with 'versionLabel' instead." #-}

instance Core.FromXML ApplicationVersionDescription where
  parseXML x =
    ApplicationVersionDescription'
      Core.<$> (x Core..@? "ApplicationName")
      Core.<*> (x Core..@? "ApplicationVersionArn")
      Core.<*> (x Core..@? "BuildArn")
      Core.<*> (x Core..@? "DateCreated")
      Core.<*> (x Core..@? "DateUpdated")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "SourceBuildInformation")
      Core.<*> (x Core..@? "SourceBundle")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "VersionLabel")
