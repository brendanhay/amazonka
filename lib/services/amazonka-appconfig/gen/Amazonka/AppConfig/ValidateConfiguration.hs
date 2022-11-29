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
-- Module      : Amazonka.AppConfig.ValidateConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses the validators in a configuration profile to validate a
-- configuration.
module Amazonka.AppConfig.ValidateConfiguration
  ( -- * Creating a Request
    ValidateConfiguration (..),
    newValidateConfiguration,

    -- * Request Lenses
    validateConfiguration_applicationId,
    validateConfiguration_configurationProfileId,
    validateConfiguration_configurationVersion,

    -- * Destructuring the Response
    ValidateConfigurationResponse (..),
    newValidateConfigurationResponse,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newValidateConfiguration' smart constructor.
data ValidateConfiguration = ValidateConfiguration'
  { -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The configuration profile ID.
    configurationProfileId :: Prelude.Text,
    -- | The version of the configuration to validate.
    configurationVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'validateConfiguration_applicationId' - The application ID.
--
-- 'configurationProfileId', 'validateConfiguration_configurationProfileId' - The configuration profile ID.
--
-- 'configurationVersion', 'validateConfiguration_configurationVersion' - The version of the configuration to validate.
newValidateConfiguration ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  -- | 'configurationVersion'
  Prelude.Text ->
  ValidateConfiguration
newValidateConfiguration
  pApplicationId_
  pConfigurationProfileId_
  pConfigurationVersion_ =
    ValidateConfiguration'
      { applicationId =
          pApplicationId_,
        configurationProfileId = pConfigurationProfileId_,
        configurationVersion = pConfigurationVersion_
      }

-- | The application ID.
validateConfiguration_applicationId :: Lens.Lens' ValidateConfiguration Prelude.Text
validateConfiguration_applicationId = Lens.lens (\ValidateConfiguration' {applicationId} -> applicationId) (\s@ValidateConfiguration' {} a -> s {applicationId = a} :: ValidateConfiguration)

-- | The configuration profile ID.
validateConfiguration_configurationProfileId :: Lens.Lens' ValidateConfiguration Prelude.Text
validateConfiguration_configurationProfileId = Lens.lens (\ValidateConfiguration' {configurationProfileId} -> configurationProfileId) (\s@ValidateConfiguration' {} a -> s {configurationProfileId = a} :: ValidateConfiguration)

-- | The version of the configuration to validate.
validateConfiguration_configurationVersion :: Lens.Lens' ValidateConfiguration Prelude.Text
validateConfiguration_configurationVersion = Lens.lens (\ValidateConfiguration' {configurationVersion} -> configurationVersion) (\s@ValidateConfiguration' {} a -> s {configurationVersion = a} :: ValidateConfiguration)

instance Core.AWSRequest ValidateConfiguration where
  type
    AWSResponse ValidateConfiguration =
      ValidateConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ValidateConfigurationResponse'

instance Prelude.Hashable ValidateConfiguration where
  hashWithSalt _salt ValidateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` configurationProfileId
      `Prelude.hashWithSalt` configurationVersion

instance Prelude.NFData ValidateConfiguration where
  rnf ValidateConfiguration' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf configurationVersion

instance Core.ToHeaders ValidateConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ValidateConfiguration where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ValidateConfiguration where
  toPath ValidateConfiguration' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/configurationprofiles/",
        Core.toBS configurationProfileId,
        "/validators"
      ]

instance Core.ToQuery ValidateConfiguration where
  toQuery ValidateConfiguration' {..} =
    Prelude.mconcat
      [ "configuration_version"
          Core.=: configurationVersion
      ]

-- | /See:/ 'newValidateConfigurationResponse' smart constructor.
data ValidateConfigurationResponse = ValidateConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newValidateConfigurationResponse ::
  ValidateConfigurationResponse
newValidateConfigurationResponse =
  ValidateConfigurationResponse'

instance Prelude.NFData ValidateConfigurationResponse where
  rnf _ = ()
