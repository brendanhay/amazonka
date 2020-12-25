{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionLocation
  ( RevisionLocation (..),

    -- * Smart constructor
    mkRevisionLocation,

    -- * Lenses
    rlAppSpecContent,
    rlGitHubLocation,
    rlRevisionType,
    rlS3Location,
    rlString,
  )
where

import qualified Network.AWS.CodeDeploy.Types.AppSpecContent as Types
import qualified Network.AWS.CodeDeploy.Types.GitHubLocation as Types
import qualified Network.AWS.CodeDeploy.Types.RawString as Types
import qualified Network.AWS.CodeDeploy.Types.RevisionLocationType as Types
import qualified Network.AWS.CodeDeploy.Types.S3Location as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the location of an application revision.
--
-- /See:/ 'mkRevisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { -- | The content of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML and stored as a RawString.
    appSpecContent :: Core.Maybe Types.AppSpecContent,
    -- | Information about the location of application artifacts stored in GitHub.
    gitHubLocation :: Core.Maybe Types.GitHubLocation,
    -- | The type of application revision:
    --
    --
    --     * S3: An application revision stored in Amazon S3.
    --
    --
    --     * GitHub: An application revision stored in GitHub (EC2/On-premises deployments only).
    --
    --
    --     * String: A YAML-formatted or JSON-formatted string (AWS Lambda deployments only).
    --
    --
    --     * AppSpecContent: An @AppSpecContent@ object that contains the contents of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML stored as a RawString.
    revisionType :: Core.Maybe Types.RevisionLocationType,
    -- | Information about the location of a revision stored in Amazon S3.
    s3Location :: Core.Maybe Types.S3Location,
    -- | Information about the location of an AWS Lambda deployment revision stored as a RawString.
    string :: Core.Maybe Types.RawString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevisionLocation' value with any optional fields omitted.
mkRevisionLocation ::
  RevisionLocation
mkRevisionLocation =
  RevisionLocation'
    { appSpecContent = Core.Nothing,
      gitHubLocation = Core.Nothing,
      revisionType = Core.Nothing,
      s3Location = Core.Nothing,
      string = Core.Nothing
    }

-- | The content of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML and stored as a RawString.
--
-- /Note:/ Consider using 'appSpecContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlAppSpecContent :: Lens.Lens' RevisionLocation (Core.Maybe Types.AppSpecContent)
rlAppSpecContent = Lens.field @"appSpecContent"
{-# DEPRECATED rlAppSpecContent "Use generic-lens or generic-optics with 'appSpecContent' instead." #-}

-- | Information about the location of application artifacts stored in GitHub.
--
-- /Note:/ Consider using 'gitHubLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlGitHubLocation :: Lens.Lens' RevisionLocation (Core.Maybe Types.GitHubLocation)
rlGitHubLocation = Lens.field @"gitHubLocation"
{-# DEPRECATED rlGitHubLocation "Use generic-lens or generic-optics with 'gitHubLocation' instead." #-}

-- | The type of application revision:
--
--
--     * S3: An application revision stored in Amazon S3.
--
--
--     * GitHub: An application revision stored in GitHub (EC2/On-premises deployments only).
--
--
--     * String: A YAML-formatted or JSON-formatted string (AWS Lambda deployments only).
--
--
--     * AppSpecContent: An @AppSpecContent@ object that contains the contents of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML stored as a RawString.
--
--
--
-- /Note:/ Consider using 'revisionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlRevisionType :: Lens.Lens' RevisionLocation (Core.Maybe Types.RevisionLocationType)
rlRevisionType = Lens.field @"revisionType"
{-# DEPRECATED rlRevisionType "Use generic-lens or generic-optics with 'revisionType' instead." #-}

-- | Information about the location of a revision stored in Amazon S3.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlS3Location :: Lens.Lens' RevisionLocation (Core.Maybe Types.S3Location)
rlS3Location = Lens.field @"s3Location"
{-# DEPRECATED rlS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | Information about the location of an AWS Lambda deployment revision stored as a RawString.
--
-- /Note:/ Consider using 'string' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlString :: Lens.Lens' RevisionLocation (Core.Maybe Types.RawString)
rlString = Lens.field @"string"
{-# DEPRECATED rlString "Use generic-lens or generic-optics with 'string' instead." #-}

instance Core.FromJSON RevisionLocation where
  toJSON RevisionLocation {..} =
    Core.object
      ( Core.catMaybes
          [ ("appSpecContent" Core..=) Core.<$> appSpecContent,
            ("gitHubLocation" Core..=) Core.<$> gitHubLocation,
            ("revisionType" Core..=) Core.<$> revisionType,
            ("s3Location" Core..=) Core.<$> s3Location,
            ("string" Core..=) Core.<$> string
          ]
      )

instance Core.FromJSON RevisionLocation where
  parseJSON =
    Core.withObject "RevisionLocation" Core.$
      \x ->
        RevisionLocation'
          Core.<$> (x Core..:? "appSpecContent")
          Core.<*> (x Core..:? "gitHubLocation")
          Core.<*> (x Core..:? "revisionType")
          Core.<*> (x Core..:? "s3Location")
          Core.<*> (x Core..:? "string")
