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
-- Module      : Amazonka.AppConfig.CreateHostedConfigurationVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configuration in the AppConfig hosted configuration store.
module Amazonka.AppConfig.CreateHostedConfigurationVersion
  ( -- * Creating a Request
    CreateHostedConfigurationVersion (..),
    newCreateHostedConfigurationVersion,

    -- * Request Lenses
    createHostedConfigurationVersion_description,
    createHostedConfigurationVersion_latestVersionNumber,
    createHostedConfigurationVersion_applicationId,
    createHostedConfigurationVersion_configurationProfileId,
    createHostedConfigurationVersion_content,
    createHostedConfigurationVersion_contentType,

    -- * Destructuring the Response
    HostedConfigurationVersion (..),
    newHostedConfigurationVersion,

    -- * Response Lenses
    hostedConfigurationVersion_applicationId,
    hostedConfigurationVersion_configurationProfileId,
    hostedConfigurationVersion_content,
    hostedConfigurationVersion_contentType,
    hostedConfigurationVersion_description,
    hostedConfigurationVersion_versionNumber,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateHostedConfigurationVersion' smart constructor.
data CreateHostedConfigurationVersion = CreateHostedConfigurationVersion'
  { -- | A description of the configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional locking token used to prevent race conditions from
    -- overwriting configuration updates when creating a new version. To ensure
    -- your data is not overwritten when creating multiple hosted configuration
    -- versions in rapid succession, specify the version number of the latest
    -- hosted configuration version.
    latestVersionNumber :: Prelude.Maybe Prelude.Int,
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The configuration profile ID.
    configurationProfileId :: Prelude.Text,
    -- | The content of the configuration or the configuration data.
    content :: Data.Sensitive Prelude.ByteString,
    -- | A standard MIME type describing the format of the configuration content.
    -- For more information, see
    -- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
    contentType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHostedConfigurationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createHostedConfigurationVersion_description' - A description of the configuration.
--
-- 'latestVersionNumber', 'createHostedConfigurationVersion_latestVersionNumber' - An optional locking token used to prevent race conditions from
-- overwriting configuration updates when creating a new version. To ensure
-- your data is not overwritten when creating multiple hosted configuration
-- versions in rapid succession, specify the version number of the latest
-- hosted configuration version.
--
-- 'applicationId', 'createHostedConfigurationVersion_applicationId' - The application ID.
--
-- 'configurationProfileId', 'createHostedConfigurationVersion_configurationProfileId' - The configuration profile ID.
--
-- 'content', 'createHostedConfigurationVersion_content' - The content of the configuration or the configuration data.
--
-- 'contentType', 'createHostedConfigurationVersion_contentType' - A standard MIME type describing the format of the configuration content.
-- For more information, see
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
newCreateHostedConfigurationVersion ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  -- | 'content'
  Prelude.ByteString ->
  -- | 'contentType'
  Prelude.Text ->
  CreateHostedConfigurationVersion
newCreateHostedConfigurationVersion
  pApplicationId_
  pConfigurationProfileId_
  pContent_
  pContentType_ =
    CreateHostedConfigurationVersion'
      { description =
          Prelude.Nothing,
        latestVersionNumber = Prelude.Nothing,
        applicationId = pApplicationId_,
        configurationProfileId =
          pConfigurationProfileId_,
        content =
          Data._Sensitive Lens.# pContent_,
        contentType = pContentType_
      }

-- | A description of the configuration.
createHostedConfigurationVersion_description :: Lens.Lens' CreateHostedConfigurationVersion (Prelude.Maybe Prelude.Text)
createHostedConfigurationVersion_description = Lens.lens (\CreateHostedConfigurationVersion' {description} -> description) (\s@CreateHostedConfigurationVersion' {} a -> s {description = a} :: CreateHostedConfigurationVersion)

-- | An optional locking token used to prevent race conditions from
-- overwriting configuration updates when creating a new version. To ensure
-- your data is not overwritten when creating multiple hosted configuration
-- versions in rapid succession, specify the version number of the latest
-- hosted configuration version.
createHostedConfigurationVersion_latestVersionNumber :: Lens.Lens' CreateHostedConfigurationVersion (Prelude.Maybe Prelude.Int)
createHostedConfigurationVersion_latestVersionNumber = Lens.lens (\CreateHostedConfigurationVersion' {latestVersionNumber} -> latestVersionNumber) (\s@CreateHostedConfigurationVersion' {} a -> s {latestVersionNumber = a} :: CreateHostedConfigurationVersion)

-- | The application ID.
createHostedConfigurationVersion_applicationId :: Lens.Lens' CreateHostedConfigurationVersion Prelude.Text
createHostedConfigurationVersion_applicationId = Lens.lens (\CreateHostedConfigurationVersion' {applicationId} -> applicationId) (\s@CreateHostedConfigurationVersion' {} a -> s {applicationId = a} :: CreateHostedConfigurationVersion)

-- | The configuration profile ID.
createHostedConfigurationVersion_configurationProfileId :: Lens.Lens' CreateHostedConfigurationVersion Prelude.Text
createHostedConfigurationVersion_configurationProfileId = Lens.lens (\CreateHostedConfigurationVersion' {configurationProfileId} -> configurationProfileId) (\s@CreateHostedConfigurationVersion' {} a -> s {configurationProfileId = a} :: CreateHostedConfigurationVersion)

-- | The content of the configuration or the configuration data.
createHostedConfigurationVersion_content :: Lens.Lens' CreateHostedConfigurationVersion Prelude.ByteString
createHostedConfigurationVersion_content = Lens.lens (\CreateHostedConfigurationVersion' {content} -> content) (\s@CreateHostedConfigurationVersion' {} a -> s {content = a} :: CreateHostedConfigurationVersion) Prelude.. Data._Sensitive

-- | A standard MIME type describing the format of the configuration content.
-- For more information, see
-- <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.17 Content-Type>.
createHostedConfigurationVersion_contentType :: Lens.Lens' CreateHostedConfigurationVersion Prelude.Text
createHostedConfigurationVersion_contentType = Lens.lens (\CreateHostedConfigurationVersion' {contentType} -> contentType) (\s@CreateHostedConfigurationVersion' {} a -> s {contentType = a} :: CreateHostedConfigurationVersion)

instance
  Core.AWSRequest
    CreateHostedConfigurationVersion
  where
  type
    AWSResponse CreateHostedConfigurationVersion =
      HostedConfigurationVersion
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          HostedConfigurationVersion'
            Prelude.<$> (h Data..#? "Application-Id")
            Prelude.<*> (h Data..#? "Configuration-Profile-Id")
            Prelude.<*> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (h Data..#? "Content-Type")
            Prelude.<*> (h Data..#? "Description")
            Prelude.<*> (h Data..#? "Version-Number")
      )

instance
  Prelude.Hashable
    CreateHostedConfigurationVersion
  where
  hashWithSalt
    _salt
    CreateHostedConfigurationVersion' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` latestVersionNumber
        `Prelude.hashWithSalt` applicationId
        `Prelude.hashWithSalt` configurationProfileId
        `Prelude.hashWithSalt` content
        `Prelude.hashWithSalt` contentType

instance
  Prelude.NFData
    CreateHostedConfigurationVersion
  where
  rnf CreateHostedConfigurationVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf latestVersionNumber
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf contentType

instance Data.ToBody CreateHostedConfigurationVersion where
  toBody CreateHostedConfigurationVersion' {..} =
    Data.toBody content

instance
  Data.ToHeaders
    CreateHostedConfigurationVersion
  where
  toHeaders CreateHostedConfigurationVersion' {..} =
    Prelude.mconcat
      [ "Description" Data.=# description,
        "Latest-Version-Number" Data.=# latestVersionNumber,
        "Content-Type" Data.=# contentType
      ]

instance Data.ToPath CreateHostedConfigurationVersion where
  toPath CreateHostedConfigurationVersion' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/configurationprofiles/",
        Data.toBS configurationProfileId,
        "/hostedconfigurationversions"
      ]

instance
  Data.ToQuery
    CreateHostedConfigurationVersion
  where
  toQuery = Prelude.const Prelude.mempty
