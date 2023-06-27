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
-- Module      : Amazonka.GuardDuty.UpdateOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the delegated administrator account with the provided values.
-- You must provide the value for either @autoEnableOrganizationMembers@ or
-- @autoEnable@.
--
-- There might be regional differences because some data sources might not
-- be available in all the Amazon Web Services Regions where GuardDuty is
-- presently supported. For more information, see
-- <https://docs.aws.amazon.com/guardduty/latest/ug/guardduty_regions.html Regions and endpoints>.
module Amazonka.GuardDuty.UpdateOrganizationConfiguration
  ( -- * Creating a Request
    UpdateOrganizationConfiguration (..),
    newUpdateOrganizationConfiguration,

    -- * Request Lenses
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfiguration_autoEnableOrganizationMembers,
    updateOrganizationConfiguration_dataSources,
    updateOrganizationConfiguration_features,
    updateOrganizationConfiguration_detectorId,

    -- * Destructuring the Response
    UpdateOrganizationConfigurationResponse (..),
    newUpdateOrganizationConfigurationResponse,

    -- * Response Lenses
    updateOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateOrganizationConfiguration' smart constructor.
data UpdateOrganizationConfiguration = UpdateOrganizationConfiguration'
  { -- | Indicates whether to automatically enable member accounts in the
    -- organization.
    --
    -- Even though this is still supported, we recommend using
    -- @AutoEnableOrganizationMembers@ to achieve the similar results.
    autoEnable :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the auto-enablement configuration of GuardDuty for the member
    -- accounts in the organization.
    --
    -- -   @NEW@: Indicates that when a new account joins the organization,
    --     they will have GuardDuty enabled automatically.
    --
    -- -   @ALL@: Indicates that all accounts in the Amazon Web Services
    --     Organization have GuardDuty enabled automatically. This includes
    --     @NEW@ accounts that join the organization and accounts that may have
    --     been suspended or removed from the organization in GuardDuty.
    --
    -- -   @NONE@: Indicates that GuardDuty will not be automatically enabled
    --     for any accounts in the organization. GuardDuty must be managed for
    --     each account individually by the administrator.
    autoEnableOrganizationMembers :: Prelude.Maybe AutoEnableMembers,
    -- | Describes which data sources will be updated.
    dataSources :: Prelude.Maybe OrganizationDataSourceConfigurations,
    -- | A list of features that will be configured for the organization.
    features :: Prelude.Maybe [OrganizationFeatureConfiguration],
    -- | The ID of the detector that configures the delegated administrator.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'updateOrganizationConfiguration_autoEnable' - Indicates whether to automatically enable member accounts in the
-- organization.
--
-- Even though this is still supported, we recommend using
-- @AutoEnableOrganizationMembers@ to achieve the similar results.
--
-- 'autoEnableOrganizationMembers', 'updateOrganizationConfiguration_autoEnableOrganizationMembers' - Indicates the auto-enablement configuration of GuardDuty for the member
-- accounts in the organization.
--
-- -   @NEW@: Indicates that when a new account joins the organization,
--     they will have GuardDuty enabled automatically.
--
-- -   @ALL@: Indicates that all accounts in the Amazon Web Services
--     Organization have GuardDuty enabled automatically. This includes
--     @NEW@ accounts that join the organization and accounts that may have
--     been suspended or removed from the organization in GuardDuty.
--
-- -   @NONE@: Indicates that GuardDuty will not be automatically enabled
--     for any accounts in the organization. GuardDuty must be managed for
--     each account individually by the administrator.
--
-- 'dataSources', 'updateOrganizationConfiguration_dataSources' - Describes which data sources will be updated.
--
-- 'features', 'updateOrganizationConfiguration_features' - A list of features that will be configured for the organization.
--
-- 'detectorId', 'updateOrganizationConfiguration_detectorId' - The ID of the detector that configures the delegated administrator.
newUpdateOrganizationConfiguration ::
  -- | 'detectorId'
  Prelude.Text ->
  UpdateOrganizationConfiguration
newUpdateOrganizationConfiguration pDetectorId_ =
  UpdateOrganizationConfiguration'
    { autoEnable =
        Prelude.Nothing,
      autoEnableOrganizationMembers =
        Prelude.Nothing,
      dataSources = Prelude.Nothing,
      features = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | Indicates whether to automatically enable member accounts in the
-- organization.
--
-- Even though this is still supported, we recommend using
-- @AutoEnableOrganizationMembers@ to achieve the similar results.
updateOrganizationConfiguration_autoEnable :: Lens.Lens' UpdateOrganizationConfiguration (Prelude.Maybe Prelude.Bool)
updateOrganizationConfiguration_autoEnable = Lens.lens (\UpdateOrganizationConfiguration' {autoEnable} -> autoEnable) (\s@UpdateOrganizationConfiguration' {} a -> s {autoEnable = a} :: UpdateOrganizationConfiguration)

-- | Indicates the auto-enablement configuration of GuardDuty for the member
-- accounts in the organization.
--
-- -   @NEW@: Indicates that when a new account joins the organization,
--     they will have GuardDuty enabled automatically.
--
-- -   @ALL@: Indicates that all accounts in the Amazon Web Services
--     Organization have GuardDuty enabled automatically. This includes
--     @NEW@ accounts that join the organization and accounts that may have
--     been suspended or removed from the organization in GuardDuty.
--
-- -   @NONE@: Indicates that GuardDuty will not be automatically enabled
--     for any accounts in the organization. GuardDuty must be managed for
--     each account individually by the administrator.
updateOrganizationConfiguration_autoEnableOrganizationMembers :: Lens.Lens' UpdateOrganizationConfiguration (Prelude.Maybe AutoEnableMembers)
updateOrganizationConfiguration_autoEnableOrganizationMembers = Lens.lens (\UpdateOrganizationConfiguration' {autoEnableOrganizationMembers} -> autoEnableOrganizationMembers) (\s@UpdateOrganizationConfiguration' {} a -> s {autoEnableOrganizationMembers = a} :: UpdateOrganizationConfiguration)

-- | Describes which data sources will be updated.
updateOrganizationConfiguration_dataSources :: Lens.Lens' UpdateOrganizationConfiguration (Prelude.Maybe OrganizationDataSourceConfigurations)
updateOrganizationConfiguration_dataSources = Lens.lens (\UpdateOrganizationConfiguration' {dataSources} -> dataSources) (\s@UpdateOrganizationConfiguration' {} a -> s {dataSources = a} :: UpdateOrganizationConfiguration)

-- | A list of features that will be configured for the organization.
updateOrganizationConfiguration_features :: Lens.Lens' UpdateOrganizationConfiguration (Prelude.Maybe [OrganizationFeatureConfiguration])
updateOrganizationConfiguration_features = Lens.lens (\UpdateOrganizationConfiguration' {features} -> features) (\s@UpdateOrganizationConfiguration' {} a -> s {features = a} :: UpdateOrganizationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the detector that configures the delegated administrator.
updateOrganizationConfiguration_detectorId :: Lens.Lens' UpdateOrganizationConfiguration Prelude.Text
updateOrganizationConfiguration_detectorId = Lens.lens (\UpdateOrganizationConfiguration' {detectorId} -> detectorId) (\s@UpdateOrganizationConfiguration' {} a -> s {detectorId = a} :: UpdateOrganizationConfiguration)

instance
  Core.AWSRequest
    UpdateOrganizationConfiguration
  where
  type
    AWSResponse UpdateOrganizationConfiguration =
      UpdateOrganizationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateOrganizationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateOrganizationConfiguration
  where
  hashWithSalt
    _salt
    UpdateOrganizationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` autoEnable
        `Prelude.hashWithSalt` autoEnableOrganizationMembers
        `Prelude.hashWithSalt` dataSources
        `Prelude.hashWithSalt` features
        `Prelude.hashWithSalt` detectorId

instance
  Prelude.NFData
    UpdateOrganizationConfiguration
  where
  rnf UpdateOrganizationConfiguration' {..} =
    Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf autoEnableOrganizationMembers
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf detectorId

instance
  Data.ToHeaders
    UpdateOrganizationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateOrganizationConfiguration where
  toJSON UpdateOrganizationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoEnable" Data..=) Prelude.<$> autoEnable,
            ("autoEnableOrganizationMembers" Data..=)
              Prelude.<$> autoEnableOrganizationMembers,
            ("dataSources" Data..=) Prelude.<$> dataSources,
            ("features" Data..=) Prelude.<$> features
          ]
      )

instance Data.ToPath UpdateOrganizationConfiguration where
  toPath UpdateOrganizationConfiguration' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId, "/admin"]

instance Data.ToQuery UpdateOrganizationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateOrganizationConfigurationResponse' smart constructor.
data UpdateOrganizationConfigurationResponse = UpdateOrganizationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newUpdateOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateOrganizationConfigurationResponse
newUpdateOrganizationConfigurationResponse
  pHttpStatus_ =
    UpdateOrganizationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateOrganizationConfigurationResponse_httpStatus :: Lens.Lens' UpdateOrganizationConfigurationResponse Prelude.Int
updateOrganizationConfigurationResponse_httpStatus = Lens.lens (\UpdateOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateOrganizationConfigurationResponse)

instance
  Prelude.NFData
    UpdateOrganizationConfigurationResponse
  where
  rnf UpdateOrganizationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
