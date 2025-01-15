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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the location of an application revision.
--
-- /See:/ 'newRevisionLocation' smart constructor.
data RevisionLocation = RevisionLocation'
  { -- | The content of an AppSpec file for an Lambda or Amazon ECS deployment.
    -- The content is formatted as JSON or YAML and stored as a RawString.
    appSpecContent :: Prelude.Maybe AppSpecContent,
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
    revisionType :: Prelude.Maybe RevisionLocationType,
    -- | Information about the location of a revision stored in Amazon S3.
    s3Location :: Prelude.Maybe S3Location,
    -- | Information about the location of an Lambda deployment revision stored
    -- as a RawString.
    string :: Prelude.Maybe RawString
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
-- 'appSpecContent', 'revisionLocation_appSpecContent' - The content of an AppSpec file for an Lambda or Amazon ECS deployment.
-- The content is formatted as JSON or YAML and stored as a RawString.
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
--
-- 's3Location', 'revisionLocation_s3Location' - Information about the location of a revision stored in Amazon S3.
--
-- 'string', 'revisionLocation_string' - Information about the location of an Lambda deployment revision stored
-- as a RawString.
newRevisionLocation ::
  RevisionLocation
newRevisionLocation =
  RevisionLocation'
    { appSpecContent = Prelude.Nothing,
      gitHubLocation = Prelude.Nothing,
      revisionType = Prelude.Nothing,
      s3Location = Prelude.Nothing,
      string = Prelude.Nothing
    }

-- | The content of an AppSpec file for an Lambda or Amazon ECS deployment.
-- The content is formatted as JSON or YAML and stored as a RawString.
revisionLocation_appSpecContent :: Lens.Lens' RevisionLocation (Prelude.Maybe AppSpecContent)
revisionLocation_appSpecContent = Lens.lens (\RevisionLocation' {appSpecContent} -> appSpecContent) (\s@RevisionLocation' {} a -> s {appSpecContent = a} :: RevisionLocation)

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

-- | Information about the location of a revision stored in Amazon S3.
revisionLocation_s3Location :: Lens.Lens' RevisionLocation (Prelude.Maybe S3Location)
revisionLocation_s3Location = Lens.lens (\RevisionLocation' {s3Location} -> s3Location) (\s@RevisionLocation' {} a -> s {s3Location = a} :: RevisionLocation)

-- | Information about the location of an Lambda deployment revision stored
-- as a RawString.
revisionLocation_string :: Lens.Lens' RevisionLocation (Prelude.Maybe RawString)
revisionLocation_string = Lens.lens (\RevisionLocation' {string} -> string) (\s@RevisionLocation' {} a -> s {string = a} :: RevisionLocation)

instance Data.FromJSON RevisionLocation where
  parseJSON =
    Data.withObject
      "RevisionLocation"
      ( \x ->
          RevisionLocation'
            Prelude.<$> (x Data..:? "appSpecContent")
            Prelude.<*> (x Data..:? "gitHubLocation")
            Prelude.<*> (x Data..:? "revisionType")
            Prelude.<*> (x Data..:? "s3Location")
            Prelude.<*> (x Data..:? "string")
      )

instance Prelude.Hashable RevisionLocation where
  hashWithSalt _salt RevisionLocation' {..} =
    _salt
      `Prelude.hashWithSalt` appSpecContent
      `Prelude.hashWithSalt` gitHubLocation
      `Prelude.hashWithSalt` revisionType
      `Prelude.hashWithSalt` s3Location
      `Prelude.hashWithSalt` string

instance Prelude.NFData RevisionLocation where
  rnf RevisionLocation' {..} =
    Prelude.rnf appSpecContent `Prelude.seq`
      Prelude.rnf gitHubLocation `Prelude.seq`
        Prelude.rnf revisionType `Prelude.seq`
          Prelude.rnf s3Location `Prelude.seq`
            Prelude.rnf string

instance Data.ToJSON RevisionLocation where
  toJSON RevisionLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("appSpecContent" Data..=)
              Prelude.<$> appSpecContent,
            ("gitHubLocation" Data..=)
              Prelude.<$> gitHubLocation,
            ("revisionType" Data..=) Prelude.<$> revisionType,
            ("s3Location" Data..=) Prelude.<$> s3Location,
            ("string" Data..=) Prelude.<$> string
          ]
      )
