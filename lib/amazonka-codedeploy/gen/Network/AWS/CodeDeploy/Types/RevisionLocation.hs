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
    rlString,
    rlRevisionType,
    rlS3Location,
    rlAppSpecContent,
    rlGitHubLocation,
  )
where

import Network.AWS.CodeDeploy.Types.AppSpecContent
import Network.AWS.CodeDeploy.Types.GitHubLocation
import Network.AWS.CodeDeploy.Types.RawString
import Network.AWS.CodeDeploy.Types.RevisionLocationType
import Network.AWS.CodeDeploy.Types.S3Location
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the location of an application revision.
--
-- /See:/ 'mkRevisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { -- | Information about the location of an AWS Lambda deployment revision stored as a RawString.
    string :: Lude.Maybe RawString,
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
    revisionType :: Lude.Maybe RevisionLocationType,
    -- | Information about the location of a revision stored in Amazon S3.
    s3Location :: Lude.Maybe S3Location,
    -- | The content of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML and stored as a RawString.
    appSpecContent :: Lude.Maybe AppSpecContent,
    -- | Information about the location of application artifacts stored in GitHub.
    gitHubLocation :: Lude.Maybe GitHubLocation
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevisionLocation' with the minimum fields required to make a request.
--
-- * 'string' - Information about the location of an AWS Lambda deployment revision stored as a RawString.
-- * 'revisionType' - The type of application revision:
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
-- * 's3Location' - Information about the location of a revision stored in Amazon S3.
-- * 'appSpecContent' - The content of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML and stored as a RawString.
-- * 'gitHubLocation' - Information about the location of application artifacts stored in GitHub.
mkRevisionLocation ::
  RevisionLocation
mkRevisionLocation =
  RevisionLocation'
    { string = Lude.Nothing,
      revisionType = Lude.Nothing,
      s3Location = Lude.Nothing,
      appSpecContent = Lude.Nothing,
      gitHubLocation = Lude.Nothing
    }

-- | Information about the location of an AWS Lambda deployment revision stored as a RawString.
--
-- /Note:/ Consider using 'string' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlString :: Lens.Lens' RevisionLocation (Lude.Maybe RawString)
rlString = Lens.lens (string :: RevisionLocation -> Lude.Maybe RawString) (\s a -> s {string = a} :: RevisionLocation)
{-# DEPRECATED rlString "Use generic-lens or generic-optics with 'string' instead." #-}

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
rlRevisionType :: Lens.Lens' RevisionLocation (Lude.Maybe RevisionLocationType)
rlRevisionType = Lens.lens (revisionType :: RevisionLocation -> Lude.Maybe RevisionLocationType) (\s a -> s {revisionType = a} :: RevisionLocation)
{-# DEPRECATED rlRevisionType "Use generic-lens or generic-optics with 'revisionType' instead." #-}

-- | Information about the location of a revision stored in Amazon S3.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlS3Location :: Lens.Lens' RevisionLocation (Lude.Maybe S3Location)
rlS3Location = Lens.lens (s3Location :: RevisionLocation -> Lude.Maybe S3Location) (\s a -> s {s3Location = a} :: RevisionLocation)
{-# DEPRECATED rlS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | The content of an AppSpec file for an AWS Lambda or Amazon ECS deployment. The content is formatted as JSON or YAML and stored as a RawString.
--
-- /Note:/ Consider using 'appSpecContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlAppSpecContent :: Lens.Lens' RevisionLocation (Lude.Maybe AppSpecContent)
rlAppSpecContent = Lens.lens (appSpecContent :: RevisionLocation -> Lude.Maybe AppSpecContent) (\s a -> s {appSpecContent = a} :: RevisionLocation)
{-# DEPRECATED rlAppSpecContent "Use generic-lens or generic-optics with 'appSpecContent' instead." #-}

-- | Information about the location of application artifacts stored in GitHub.
--
-- /Note:/ Consider using 'gitHubLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlGitHubLocation :: Lens.Lens' RevisionLocation (Lude.Maybe GitHubLocation)
rlGitHubLocation = Lens.lens (gitHubLocation :: RevisionLocation -> Lude.Maybe GitHubLocation) (\s a -> s {gitHubLocation = a} :: RevisionLocation)
{-# DEPRECATED rlGitHubLocation "Use generic-lens or generic-optics with 'gitHubLocation' instead." #-}

instance Lude.FromJSON RevisionLocation where
  parseJSON =
    Lude.withObject
      "RevisionLocation"
      ( \x ->
          RevisionLocation'
            Lude.<$> (x Lude..:? "string")
            Lude.<*> (x Lude..:? "revisionType")
            Lude.<*> (x Lude..:? "s3Location")
            Lude.<*> (x Lude..:? "appSpecContent")
            Lude.<*> (x Lude..:? "gitHubLocation")
      )

instance Lude.ToJSON RevisionLocation where
  toJSON RevisionLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("string" Lude..=) Lude.<$> string,
            ("revisionType" Lude..=) Lude.<$> revisionType,
            ("s3Location" Lude..=) Lude.<$> s3Location,
            ("appSpecContent" Lude..=) Lude.<$> appSpecContent,
            ("gitHubLocation" Lude..=) Lude.<$> gitHubLocation
          ]
      )
