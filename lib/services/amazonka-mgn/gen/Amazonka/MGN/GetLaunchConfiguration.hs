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
-- Module      : Amazonka.MGN.GetLaunchConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all LaunchConfigurations available, filtered by Source Server IDs.
module Amazonka.MGN.GetLaunchConfiguration
  ( -- * Creating a Request
    GetLaunchConfiguration (..),
    newGetLaunchConfiguration,

    -- * Request Lenses
    getLaunchConfiguration_sourceServerID,

    -- * Destructuring the Response
    LaunchConfiguration (..),
    newLaunchConfiguration,

    -- * Response Lenses
    launchConfiguration_bootMode,
    launchConfiguration_copyPrivateIp,
    launchConfiguration_copyTags,
    launchConfiguration_ec2LaunchTemplateID,
    launchConfiguration_enableMapAutoTagging,
    launchConfiguration_launchDisposition,
    launchConfiguration_licensing,
    launchConfiguration_mapAutoTaggingMpeID,
    launchConfiguration_name,
    launchConfiguration_postLaunchActions,
    launchConfiguration_sourceServerID,
    launchConfiguration_targetInstanceTypeRightSizingMethod,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLaunchConfiguration' smart constructor.
data GetLaunchConfiguration = GetLaunchConfiguration'
  { -- | Request to get Launch Configuration information by Source Server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLaunchConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'getLaunchConfiguration_sourceServerID' - Request to get Launch Configuration information by Source Server ID.
newGetLaunchConfiguration ::
  -- | 'sourceServerID'
  Prelude.Text ->
  GetLaunchConfiguration
newGetLaunchConfiguration pSourceServerID_ =
  GetLaunchConfiguration'
    { sourceServerID =
        pSourceServerID_
    }

-- | Request to get Launch Configuration information by Source Server ID.
getLaunchConfiguration_sourceServerID :: Lens.Lens' GetLaunchConfiguration Prelude.Text
getLaunchConfiguration_sourceServerID = Lens.lens (\GetLaunchConfiguration' {sourceServerID} -> sourceServerID) (\s@GetLaunchConfiguration' {} a -> s {sourceServerID = a} :: GetLaunchConfiguration)

instance Core.AWSRequest GetLaunchConfiguration where
  type
    AWSResponse GetLaunchConfiguration =
      LaunchConfiguration
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetLaunchConfiguration where
  hashWithSalt _salt GetLaunchConfiguration' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData GetLaunchConfiguration where
  rnf GetLaunchConfiguration' {..} =
    Prelude.rnf sourceServerID

instance Data.ToHeaders GetLaunchConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLaunchConfiguration where
  toJSON GetLaunchConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath GetLaunchConfiguration where
  toPath = Prelude.const "/GetLaunchConfiguration"

instance Data.ToQuery GetLaunchConfiguration where
  toQuery = Prelude.const Prelude.mempty
