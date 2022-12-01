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
-- Module      : Amazonka.CodeDeploy.Types.RevisionLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.RevisionLocation where

import Amazonka.CodeDeploy.Types.AppSpecContent
import Amazonka.CodeDeploy.Types.GitHubLocation
import Amazonka.CodeDeploy.Types.RawString
import Amazonka.CodeDeploy.Types.RevisionLocationType
import Amazonka.CodeDeploy.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the location of an application revision.
--
-- /See:/ 'newRevisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { -- | Information about the location of an Lambda deployment revision stored
    -- as a RawString.
    string :: Prelude.Maybe RawString,
    -- | The content of an AppSpec file for an Lambda or Amazon ECS deployment.
    -- The content is formatted as JSON or YAML and stored as a RawString.
    appSpecContent :: Prelude.Maybe AppSpecContent,
    -- | Information about the location of a revision stored in Amazon S3.
    s3Location :: Prelude.Maybe S3Location,
    -- | Information about the location of application artifacts stored in
    -- GitHub.
    gitHubLocation :: Prelude.Maybe GitHubLocation,
    -- | The type of application revision:
    --
    -- -   S3: An application revision stored in Amazon S3.
    --
    -- -   GitHub: An application revision stored in GitHub (EC2\/On-premises
    --     deployments only).
    --
    -- -   String: A YAML-formatted or JSON-formatted string (Lambda
    --     deployments only).
    --
    -- -   AppSpecContent: An @AppSpecContent@ object that contains the
    --     contents of an AppSpec file for an Lambda or Amazon ECS deployment.
    --     The content is formatted as JSON or YAML stored as a RawString.
    revisionType :: Prelude.Maybe RevisionLocationType
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
-- 'string', 'revisionLocation_string' - Information about the location of an Lambda deployment revision stored
-- as a RawString.
--
-- 'appSpecContent', 'revisionLocation_appSpecContent' - The content of an AppSpec file for an Lambda or Amazon ECS deployment.
-- The content is formatted as JSON or YAML and stored as a RawString.
--
-- 's3Location', 'revisionLocation_s3Location' - Information about the location of a revision stored in Amazon S3.
--
-- 'gitHubLocation', 'revisionLocation_gitHubLocation' - Information about the location of application artifacts stored in
-- GitHub.
--
-- 'revisionType', 'revisionLocation_revisionType' - The type of application revision:
--
-- -   S3: An application revision stored in Amazon S3.
--
-- -   GitHub: An application revision stored in GitHub (EC2\/On-premises
--     deployments only).
--
-- -   String: A YAML-formatted or JSON-formatted string (Lambda
--     deployments only).
--
-- -   AppSpecContent: An @AppSpecContent@ object that contains the
--     contents of an AppSpec file for an Lambda or Amazon ECS deployment.
--     The content is formatted as JSON or YAML stored as a RawString.
newRevisionLocation ::
  RevisionLocation
newRevisionLocation =
  RevisionLocation'
    { string = Prelude.Nothing,
      appSpecContent = Prelude.Nothing,
      s3Location = Prelude.Nothing,
      gitHubLocation = Prelude.Nothing,
      revisionType = Prelude.Nothing
    }

-- | Information about the location of an Lambda deployment revision stored
-- as a RawString.
revisionLocation_string :: Lens.Lens' RevisionLocation (Prelude.Maybe RawString)
revisionLocation_string = Lens.lens (\RevisionLocation' {string} -> string) (\s@RevisionLocation' {} a -> s {string = a} :: RevisionLocation)

-- | The content of an AppSpec file for an Lambda or Amazon ECS deployment.
-- The content is formatted as JSON or YAML and stored as a RawString.
revisionLocation_appSpecContent :: Lens.Lens' RevisionLocation (Prelude.Maybe AppSpecContent)
revisionLocation_appSpecContent = Lens.lens (\RevisionLocation' {appSpecContent} -> appSpecContent) (\s@RevisionLocation' {} a -> s {appSpecContent = a} :: RevisionLocation)

-- | Information about the location of a revision stored in Amazon S3.
revisionLocation_s3Location :: Lens.Lens' RevisionLocation (Prelude.Maybe S3Location)
revisionLocation_s3Location = Lens.lens (\RevisionLocation' {s3Location} -> s3Location) (\s@RevisionLocation' {} a -> s {s3Location = a} :: RevisionLocation)

-- | Information about the location of application artifacts stored in
-- GitHub.
revisionLocation_gitHubLocation :: Lens.Lens' RevisionLocation (Prelude.Maybe GitHubLocation)
revisionLocation_gitHubLocation = Lens.lens (\RevisionLocation' {gitHubLocation} -> gitHubLocation) (\s@RevisionLocation' {} a -> s {gitHubLocation = a} :: RevisionLocation)

-- | The type of application revision:
--
-- -   S3: An application revision stored in Amazon S3.
--
-- -   GitHub: An application revision stored in GitHub (EC2\/On-premises
--     deployments only).
--
-- -   String: A YAML-formatted or JSON-formatted string (Lambda
--     deployments only).
--
-- -   AppSpecContent: An @AppSpecContent@ object that contains the
--     contents of an AppSpec file for an Lambda or Amazon ECS deployment.
--     The content is formatted as JSON or YAML stored as a RawString.
revisionLocation_revisionType :: Lens.Lens' RevisionLocation (Prelude.Maybe RevisionLocationType)
revisionLocation_revisionType = Lens.lens (\RevisionLocation' {revisionType} -> revisionType) (\s@RevisionLocation' {} a -> s {revisionType = a} :: RevisionLocation)

instance Core.FromJSON RevisionLocation where
  parseJSON =
    Core.withObject
      "RevisionLocation"
      ( \x ->
          RevisionLocation'
            Prelude.<$> (x Core..:? "string")
            Prelude.<*> (x Core..:? "appSpecContent")
            Prelude.<*> (x Core..:? "s3Location")
            Prelude.<*> (x Core..:? "gitHubLocation")
            Prelude.<*> (x Core..:? "revisionType")
      )

instance Prelude.Hashable RevisionLocation where
  hashWithSalt _salt RevisionLocation' {..} =
    _salt `Prelude.hashWithSalt` string
      `Prelude.hashWithSalt` appSpecContent
      `Prelude.hashWithSalt` s3Location
      `Prelude.hashWithSalt` gitHubLocation
      `Prelude.hashWithSalt` revisionType

instance Prelude.NFData RevisionLocation where
  rnf RevisionLocation' {..} =
    Prelude.rnf string
      `Prelude.seq` Prelude.rnf appSpecContent
      `Prelude.seq` Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf gitHubLocation
      `Prelude.seq` Prelude.rnf revisionType

instance Core.ToJSON RevisionLocation where
  toJSON RevisionLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("string" Core..=) Prelude.<$> string,
            ("appSpecContent" Core..=)
              Prelude.<$> appSpecContent,
            ("s3Location" Core..=) Prelude.<$> s3Location,
            ("gitHubLocation" Core..=)
              Prelude.<$> gitHubLocation,
            ("revisionType" Core..=) Prelude.<$> revisionType
          ]
      )
