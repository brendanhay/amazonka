{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ElasticBeanstalk.ComposeEnvironments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or update a group of environments that each run a separate
-- component of a single application. Takes a list of version labels that
-- specify application source bundles for each of the environments to
-- create or update. The name of each environment and other required
-- information must be included in the source bundles in an environment
-- manifest named @env.yaml@. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-mgmt-compose.html Compose Environments>
-- for details.
module Amazonka.ElasticBeanstalk.ComposeEnvironments
  ( -- * Creating a Request
    ComposeEnvironments (..),
    newComposeEnvironments,

    -- * Request Lenses
    composeEnvironments_applicationName,
    composeEnvironments_groupName,
    composeEnvironments_versionLabels,

    -- * Destructuring the Response
    EnvironmentDescriptionsMessage (..),
    newEnvironmentDescriptionsMessage,

    -- * Response Lenses
    environmentDescriptionsMessage_environments,
    environmentDescriptionsMessage_nextToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to create or update a group of environments.
--
-- /See:/ 'newComposeEnvironments' smart constructor.
data ComposeEnvironments = ComposeEnvironments'
  { -- | The name of the application to which the specified source bundles
    -- belong.
    applicationName :: Prelude.Maybe Prelude.Text,
    -- | The name of the group to which the target environments belong. Specify a
    -- group name only if the environment name defined in each target
    -- environment\'s manifest ends with a + (plus) character. See
    -- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
    -- for details.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | A list of version labels, specifying one or more application source
    -- bundles that belong to the target application. Each source bundle must
    -- include an environment manifest that specifies the name of the
    -- environment and the name of the solution stack to use, and optionally
    -- can specify environment links to create.
    versionLabels :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComposeEnvironments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'composeEnvironments_applicationName' - The name of the application to which the specified source bundles
-- belong.
--
-- 'groupName', 'composeEnvironments_groupName' - The name of the group to which the target environments belong. Specify a
-- group name only if the environment name defined in each target
-- environment\'s manifest ends with a + (plus) character. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
--
-- 'versionLabels', 'composeEnvironments_versionLabels' - A list of version labels, specifying one or more application source
-- bundles that belong to the target application. Each source bundle must
-- include an environment manifest that specifies the name of the
-- environment and the name of the solution stack to use, and optionally
-- can specify environment links to create.
newComposeEnvironments ::
  ComposeEnvironments
newComposeEnvironments =
  ComposeEnvironments'
    { applicationName =
        Prelude.Nothing,
      groupName = Prelude.Nothing,
      versionLabels = Prelude.Nothing
    }

-- | The name of the application to which the specified source bundles
-- belong.
composeEnvironments_applicationName :: Lens.Lens' ComposeEnvironments (Prelude.Maybe Prelude.Text)
composeEnvironments_applicationName = Lens.lens (\ComposeEnvironments' {applicationName} -> applicationName) (\s@ComposeEnvironments' {} a -> s {applicationName = a} :: ComposeEnvironments)

-- | The name of the group to which the target environments belong. Specify a
-- group name only if the environment name defined in each target
-- environment\'s manifest ends with a + (plus) character. See
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/environment-cfg-manifest.html Environment Manifest (env.yaml)>
-- for details.
composeEnvironments_groupName :: Lens.Lens' ComposeEnvironments (Prelude.Maybe Prelude.Text)
composeEnvironments_groupName = Lens.lens (\ComposeEnvironments' {groupName} -> groupName) (\s@ComposeEnvironments' {} a -> s {groupName = a} :: ComposeEnvironments)

-- | A list of version labels, specifying one or more application source
-- bundles that belong to the target application. Each source bundle must
-- include an environment manifest that specifies the name of the
-- environment and the name of the solution stack to use, and optionally
-- can specify environment links to create.
composeEnvironments_versionLabels :: Lens.Lens' ComposeEnvironments (Prelude.Maybe [Prelude.Text])
composeEnvironments_versionLabels = Lens.lens (\ComposeEnvironments' {versionLabels} -> versionLabels) (\s@ComposeEnvironments' {} a -> s {versionLabels = a} :: ComposeEnvironments) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ComposeEnvironments where
  type
    AWSResponse ComposeEnvironments =
      EnvironmentDescriptionsMessage
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ComposeEnvironmentsResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ComposeEnvironments where
  hashWithSalt _salt ComposeEnvironments' {..} =
    _salt `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` versionLabels

instance Prelude.NFData ComposeEnvironments where
  rnf ComposeEnvironments' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf versionLabels

instance Data.ToHeaders ComposeEnvironments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ComposeEnvironments where
  toPath = Prelude.const "/"

instance Data.ToQuery ComposeEnvironments where
  toQuery ComposeEnvironments' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ComposeEnvironments" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "ApplicationName" Data.=: applicationName,
        "GroupName" Data.=: groupName,
        "VersionLabels"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> versionLabels
            )
      ]
