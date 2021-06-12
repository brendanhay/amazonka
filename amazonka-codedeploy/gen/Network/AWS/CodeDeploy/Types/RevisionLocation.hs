{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.RevisionLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.RevisionLocation where

import Network.AWS.CodeDeploy.Types.AppSpecContent
import Network.AWS.CodeDeploy.Types.GitHubLocation
import Network.AWS.CodeDeploy.Types.RawString
import Network.AWS.CodeDeploy.Types.RevisionLocationType
import Network.AWS.CodeDeploy.Types.S3Location
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the location of an application revision.
--
-- /See:/ 'newRevisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { -- | The type of application revision:
    --
    -- -   S3: An application revision stored in Amazon S3.
    --
    -- -   GitHub: An application revision stored in GitHub (EC2\/On-premises
    --     deployments only).
    --
    -- -   String: A YAML-formatted or JSON-formatted string (AWS Lambda
    --     deployments only).
    --
    -- -   AppSpecContent: An @AppSpecContent@ object that contains the
    --     contents of an AppSpec file for an AWS Lambda or Amazon ECS
    --     deployment. The content is formatted as JSON or YAML stored as a
    --     RawString.
    revisionType :: Core.Maybe RevisionLocationType,
    -- | Information about the location of a revision stored in Amazon S3.
    s3Location :: Core.Maybe S3Location,
    -- | The content of an AppSpec file for an AWS Lambda or Amazon ECS
    -- deployment. The content is formatted as JSON or YAML and stored as a
    -- RawString.
    appSpecContent :: Core.Maybe AppSpecContent,
    -- | Information about the location of application artifacts stored in
    -- GitHub.
    gitHubLocation :: Core.Maybe GitHubLocation,
    -- | Information about the location of an AWS Lambda deployment revision
    -- stored as a RawString.
    string :: Core.Maybe RawString
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RevisionLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionType', 'revisionLocation_revisionType' - The type of application revision:
--
-- -   S3: An application revision stored in Amazon S3.
--
-- -   GitHub: An application revision stored in GitHub (EC2\/On-premises
--     deployments only).
--
-- -   String: A YAML-formatted or JSON-formatted string (AWS Lambda
--     deployments only).
--
-- -   AppSpecContent: An @AppSpecContent@ object that contains the
--     contents of an AppSpec file for an AWS Lambda or Amazon ECS
--     deployment. The content is formatted as JSON or YAML stored as a
--     RawString.
--
-- 's3Location', 'revisionLocation_s3Location' - Information about the location of a revision stored in Amazon S3.
--
-- 'appSpecContent', 'revisionLocation_appSpecContent' - The content of an AppSpec file for an AWS Lambda or Amazon ECS
-- deployment. The content is formatted as JSON or YAML and stored as a
-- RawString.
--
-- 'gitHubLocation', 'revisionLocation_gitHubLocation' - Information about the location of application artifacts stored in
-- GitHub.
--
-- 'string', 'revisionLocation_string' - Information about the location of an AWS Lambda deployment revision
-- stored as a RawString.
newRevisionLocation ::
  RevisionLocation
newRevisionLocation =
  RevisionLocation'
    { revisionType = Core.Nothing,
      s3Location = Core.Nothing,
      appSpecContent = Core.Nothing,
      gitHubLocation = Core.Nothing,
      string = Core.Nothing
    }

-- | The type of application revision:
--
-- -   S3: An application revision stored in Amazon S3.
--
-- -   GitHub: An application revision stored in GitHub (EC2\/On-premises
--     deployments only).
--
-- -   String: A YAML-formatted or JSON-formatted string (AWS Lambda
--     deployments only).
--
-- -   AppSpecContent: An @AppSpecContent@ object that contains the
--     contents of an AppSpec file for an AWS Lambda or Amazon ECS
--     deployment. The content is formatted as JSON or YAML stored as a
--     RawString.
revisionLocation_revisionType :: Lens.Lens' RevisionLocation (Core.Maybe RevisionLocationType)
revisionLocation_revisionType = Lens.lens (\RevisionLocation' {revisionType} -> revisionType) (\s@RevisionLocation' {} a -> s {revisionType = a} :: RevisionLocation)

-- | Information about the location of a revision stored in Amazon S3.
revisionLocation_s3Location :: Lens.Lens' RevisionLocation (Core.Maybe S3Location)
revisionLocation_s3Location = Lens.lens (\RevisionLocation' {s3Location} -> s3Location) (\s@RevisionLocation' {} a -> s {s3Location = a} :: RevisionLocation)

-- | The content of an AppSpec file for an AWS Lambda or Amazon ECS
-- deployment. The content is formatted as JSON or YAML and stored as a
-- RawString.
revisionLocation_appSpecContent :: Lens.Lens' RevisionLocation (Core.Maybe AppSpecContent)
revisionLocation_appSpecContent = Lens.lens (\RevisionLocation' {appSpecContent} -> appSpecContent) (\s@RevisionLocation' {} a -> s {appSpecContent = a} :: RevisionLocation)

-- | Information about the location of application artifacts stored in
-- GitHub.
revisionLocation_gitHubLocation :: Lens.Lens' RevisionLocation (Core.Maybe GitHubLocation)
revisionLocation_gitHubLocation = Lens.lens (\RevisionLocation' {gitHubLocation} -> gitHubLocation) (\s@RevisionLocation' {} a -> s {gitHubLocation = a} :: RevisionLocation)

-- | Information about the location of an AWS Lambda deployment revision
-- stored as a RawString.
revisionLocation_string :: Lens.Lens' RevisionLocation (Core.Maybe RawString)
revisionLocation_string = Lens.lens (\RevisionLocation' {string} -> string) (\s@RevisionLocation' {} a -> s {string = a} :: RevisionLocation)

instance Core.FromJSON RevisionLocation where
  parseJSON =
    Core.withObject
      "RevisionLocation"
      ( \x ->
          RevisionLocation'
            Core.<$> (x Core..:? "revisionType")
            Core.<*> (x Core..:? "s3Location")
            Core.<*> (x Core..:? "appSpecContent")
            Core.<*> (x Core..:? "gitHubLocation")
            Core.<*> (x Core..:? "string")
      )

instance Core.Hashable RevisionLocation

instance Core.NFData RevisionLocation

instance Core.ToJSON RevisionLocation where
  toJSON RevisionLocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("revisionType" Core..=) Core.<$> revisionType,
            ("s3Location" Core..=) Core.<$> s3Location,
            ("appSpecContent" Core..=) Core.<$> appSpecContent,
            ("gitHubLocation" Core..=) Core.<$> gitHubLocation,
            ("string" Core..=) Core.<$> string
          ]
      )
