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
-- Module      : Amazonka.AppConfig.GetHostedConfigurationVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specific configuration version.
module Amazonka.AppConfig.GetHostedConfigurationVersion
  ( -- * Creating a Request
    GetHostedConfigurationVersion (..),
    newGetHostedConfigurationVersion,

    -- * Request Lenses
    getHostedConfigurationVersion_applicationId,
    getHostedConfigurationVersion_configurationProfileId,
    getHostedConfigurationVersion_versionNumber,

    -- * Destructuring the Response
    HostedConfigurationVersion (..),
    newHostedConfigurationVersion,

    -- * Response Lenses
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_contentType,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHostedConfigurationVersion' smart constructor.
data GetHostedConfigurationVersion = GetHostedConfigurationVersion'
  { -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The configuration profile ID.
    configurationProfileId :: Prelude.Text,
    -- | The version.
    versionNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHostedConfigurationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getHostedConfigurationVersion_applicationId' - The application ID.
--
-- 'configurationProfileId', 'getHostedConfigurationVersion_configurationProfileId' - The configuration profile ID.
--
-- 'versionNumber', 'getHostedConfigurationVersion_versionNumber' - The version.
newGetHostedConfigurationVersion ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  -- | 'versionNumber'
  Prelude.Int ->
  GetHostedConfigurationVersion
newGetHostedConfigurationVersion
  pApplicationId_
  pConfigurationProfileId_
  pVersionNumber_ =
    GetHostedConfigurationVersion'
      { applicationId =
          pApplicationId_,
        configurationProfileId =
          pConfigurationProfileId_,
        versionNumber = pVersionNumber_
      }

-- | The application ID.
getHostedConfigurationVersion_applicationId :: Lens.Lens' GetHostedConfigurationVersion Prelude.Text
getHostedConfigurationVersion_applicationId = Lens.lens (\GetHostedConfigurationVersion' {applicationId} -> applicationId) (\s@GetHostedConfigurationVersion' {} a -> s {applicationId = a} :: GetHostedConfigurationVersion)

-- | The configuration profile ID.
getHostedConfigurationVersion_configurationProfileId :: Lens.Lens' GetHostedConfigurationVersion Prelude.Text
getHostedConfigurationVersion_configurationProfileId = Lens.lens (\GetHostedConfigurationVersion' {configurationProfileId} -> configurationProfileId) (\s@GetHostedConfigurationVersion' {} a -> s {configurationProfileId = a} :: GetHostedConfigurationVersion)

-- | The version.
getHostedConfigurationVersion_versionNumber :: Lens.Lens' GetHostedConfigurationVersion Prelude.Int
getHostedConfigurationVersion_versionNumber = Lens.lens (\GetHostedConfigurationVersion' {versionNumber} -> versionNumber) (\s@GetHostedConfigurationVersion' {} a -> s {versionNumber = a} :: GetHostedConfigurationVersion)

instance
  Core.AWSRequest
    GetHostedConfigurationVersion
  where
  type
    AWSResponse GetHostedConfigurationVersion =
      HostedConfigurationVersion
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          HostedConfigurationVersion'
            Prelude.<$> (h Core..#? "Description")
            Prelude.<*> (h Core..#? "Version-Number")
            Prelude.<*> (h Core..#? "Application-Id")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Core..#? "Configuration-Profile-Id")
            Prelude.<*> (h Core..#? "Content-Type")
      )

instance
  Prelude.Hashable
    GetHostedConfigurationVersion
  where
  hashWithSalt _salt GetHostedConfigurationVersion' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` configurationProfileId
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData GetHostedConfigurationVersion where
  rnf GetHostedConfigurationVersion' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf versionNumber

instance Core.ToHeaders GetHostedConfigurationVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetHostedConfigurationVersion where
  toPath GetHostedConfigurationVersion' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/configurationprofiles/",
        Core.toBS configurationProfileId,
        "/hostedconfigurationversions/",
        Core.toBS versionNumber
      ]

instance Core.ToQuery GetHostedConfigurationVersion where
  toQuery = Prelude.const Prelude.mempty
