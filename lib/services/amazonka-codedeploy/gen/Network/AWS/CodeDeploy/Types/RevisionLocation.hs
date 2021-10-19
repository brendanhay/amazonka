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
import qualified Network.AWS.Prelude as Prelude

-- | Information about the location of an application revision.
--
-- /See:/ 'newRevisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { -- | Information about the location of an AWS Lambda deployment revision
    -- stored as a RawString.
    string :: Prelude.Maybe RawString,
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
    revisionType :: Prelude.Maybe RevisionLocationType,
    -- | Information about the location of a revision stored in Amazon S3.
    s3Location :: Prelude.Maybe S3Location,
    -- | The content of an AppSpec file for an AWS Lambda or Amazon ECS
    -- deployment. The content is formatted as JSON or YAML and stored as a
    -- RawString.
    appSpecContent :: Prelude.Maybe AppSpecContent,
    -- | Information about the location of application artifacts stored in
    -- GitHub.
    gitHubLocation :: Prelude.Maybe GitHubLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevisionLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'string', 'revisionLocation_string' - Information about the location of an AWS Lambda deployment revision
-- stored as a RawString.
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
newRevisionLocation ::
  RevisionLocation
newRevisionLocation =
  RevisionLocation'
    { string = Prelude.Nothing,
      revisionType = Prelude.Nothing,
      s3Location = Prelude.Nothing,
      appSpecContent = Prelude.Nothing,
      gitHubLocation = Prelude.Nothing
    }

-- | Information about the location of an AWS Lambda deployment revision
-- stored as a RawString.
revisionLocation_string :: Lens.Lens' RevisionLocation (Prelude.Maybe RawString)
revisionLocation_string = Lens.lens (\RevisionLocation' {string} -> string) (\s@RevisionLocation' {} a -> s {string = a} :: RevisionLocation)

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
revisionLocation_revisionType :: Lens.Lens' RevisionLocation (Prelude.Maybe RevisionLocationType)
revisionLocation_revisionType = Lens.lens (\RevisionLocation' {revisionType} -> revisionType) (\s@RevisionLocation' {} a -> s {revisionType = a} :: RevisionLocation)

-- | Information about the location of a revision stored in Amazon S3.
revisionLocation_s3Location :: Lens.Lens' RevisionLocation (Prelude.Maybe S3Location)
revisionLocation_s3Location = Lens.lens (\RevisionLocation' {s3Location} -> s3Location) (\s@RevisionLocation' {} a -> s {s3Location = a} :: RevisionLocation)

-- | The content of an AppSpec file for an AWS Lambda or Amazon ECS
-- deployment. The content is formatted as JSON or YAML and stored as a
-- RawString.
revisionLocation_appSpecContent :: Lens.Lens' RevisionLocation (Prelude.Maybe AppSpecContent)
revisionLocation_appSpecContent = Lens.lens (\RevisionLocation' {appSpecContent} -> appSpecContent) (\s@RevisionLocation' {} a -> s {appSpecContent = a} :: RevisionLocation)

-- | Information about the location of application artifacts stored in
-- GitHub.
revisionLocation_gitHubLocation :: Lens.Lens' RevisionLocation (Prelude.Maybe GitHubLocation)
revisionLocation_gitHubLocation = Lens.lens (\RevisionLocation' {gitHubLocation} -> gitHubLocation) (\s@RevisionLocation' {} a -> s {gitHubLocation = a} :: RevisionLocation)

instance Core.FromJSON RevisionLocation where
  parseJSON =
    Core.withObject
      "RevisionLocation"
      ( \x ->
          RevisionLocation'
            Prelude.<$> (x Core..:? "string")
            Prelude.<*> (x Core..:? "revisionType")
            Prelude.<*> (x Core..:? "s3Location")
            Prelude.<*> (x Core..:? "appSpecContent")
            Prelude.<*> (x Core..:? "gitHubLocation")
      )

instance Prelude.Hashable RevisionLocation

instance Prelude.NFData RevisionLocation

instance Core.ToJSON RevisionLocation where
  toJSON RevisionLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("string" Core..=) Prelude.<$> string,
            ("revisionType" Core..=) Prelude.<$> revisionType,
            ("s3Location" Core..=) Prelude.<$> s3Location,
            ("appSpecContent" Core..=)
              Prelude.<$> appSpecContent,
            ("gitHubLocation" Core..=)
              Prelude.<$> gitHubLocation
          ]
      )
