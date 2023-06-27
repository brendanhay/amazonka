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
-- Module      : Amazonka.GuardDuty.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the account selected as the delegated
-- administrator for GuardDuty.
--
-- There might be regional differences because some data sources might not
-- be available in all the Amazon Web Services Regions where GuardDuty is
-- presently supported. For more information, see
-- <https://docs.aws.amazon.com/guardduty/latest/ug/guardduty_regions.html Regions and endpoints>.
module Amazonka.GuardDuty.DescribeOrganizationConfiguration
  ( -- * Creating a Request
    DescribeOrganizationConfiguration (..),
    newDescribeOrganizationConfiguration,

    -- * Request Lenses
    describeOrganizationConfiguration_maxResults,
    describeOrganizationConfiguration_nextToken,
    describeOrganizationConfiguration_detectorId,

    -- * Destructuring the Response
    DescribeOrganizationConfigurationResponse (..),
    newDescribeOrganizationConfigurationResponse,

    -- * Response Lenses
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_autoEnableOrganizationMembers,
    describeOrganizationConfigurationResponse_dataSources,
    describeOrganizationConfigurationResponse_features,
    describeOrganizationConfigurationResponse_nextToken,
    describeOrganizationConfigurationResponse_httpStatus,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationConfiguration' smart constructor.
data DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  { -- | You can use this parameter to indicate the maximum number of items that
    -- you want in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | You can use this parameter when paginating results. Set the value of
    -- this parameter to null on your first call to the list action. For
    -- subsequent calls to the action, fill @nextToken@ in the request with the
    -- value of @NextToken@ from the previous response to continue listing
    -- data.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the detector to retrieve information about the delegated
    -- administrator from.
    detectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeOrganizationConfiguration_maxResults' - You can use this parameter to indicate the maximum number of items that
-- you want in the response.
--
-- 'nextToken', 'describeOrganizationConfiguration_nextToken' - You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill @nextToken@ in the request with the
-- value of @NextToken@ from the previous response to continue listing
-- data.
--
-- 'detectorId', 'describeOrganizationConfiguration_detectorId' - The ID of the detector to retrieve information about the delegated
-- administrator from.
newDescribeOrganizationConfiguration ::
  -- | 'detectorId'
  Prelude.Text ->
  DescribeOrganizationConfiguration
newDescribeOrganizationConfiguration pDetectorId_ =
  DescribeOrganizationConfiguration'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      detectorId = pDetectorId_
    }

-- | You can use this parameter to indicate the maximum number of items that
-- you want in the response.
describeOrganizationConfiguration_maxResults :: Lens.Lens' DescribeOrganizationConfiguration (Prelude.Maybe Prelude.Natural)
describeOrganizationConfiguration_maxResults = Lens.lens (\DescribeOrganizationConfiguration' {maxResults} -> maxResults) (\s@DescribeOrganizationConfiguration' {} a -> s {maxResults = a} :: DescribeOrganizationConfiguration)

-- | You can use this parameter when paginating results. Set the value of
-- this parameter to null on your first call to the list action. For
-- subsequent calls to the action, fill @nextToken@ in the request with the
-- value of @NextToken@ from the previous response to continue listing
-- data.
describeOrganizationConfiguration_nextToken :: Lens.Lens' DescribeOrganizationConfiguration (Prelude.Maybe Prelude.Text)
describeOrganizationConfiguration_nextToken = Lens.lens (\DescribeOrganizationConfiguration' {nextToken} -> nextToken) (\s@DescribeOrganizationConfiguration' {} a -> s {nextToken = a} :: DescribeOrganizationConfiguration)

-- | The ID of the detector to retrieve information about the delegated
-- administrator from.
describeOrganizationConfiguration_detectorId :: Lens.Lens' DescribeOrganizationConfiguration Prelude.Text
describeOrganizationConfiguration_detectorId = Lens.lens (\DescribeOrganizationConfiguration' {detectorId} -> detectorId) (\s@DescribeOrganizationConfiguration' {} a -> s {detectorId = a} :: DescribeOrganizationConfiguration)

instance
  Core.AWSRequest
    DescribeOrganizationConfiguration
  where
  type
    AWSResponse DescribeOrganizationConfiguration =
      DescribeOrganizationConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigurationResponse'
            Prelude.<$> (x Data..?> "autoEnable")
            Prelude.<*> (x Data..?> "autoEnableOrganizationMembers")
            Prelude.<*> (x Data..?> "dataSources")
            Prelude.<*> (x Data..?> "features" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "memberAccountLimitReached")
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfiguration
  where
  hashWithSalt
    _salt
    DescribeOrganizationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` detectorId

instance
  Prelude.NFData
    DescribeOrganizationConfiguration
  where
  rnf DescribeOrganizationConfiguration' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf detectorId

instance
  Data.ToHeaders
    DescribeOrganizationConfiguration
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

instance
  Data.ToPath
    DescribeOrganizationConfiguration
  where
  toPath DescribeOrganizationConfiguration' {..} =
    Prelude.mconcat
      ["/detector/", Data.toBS detectorId, "/admin"]

instance
  Data.ToQuery
    DescribeOrganizationConfiguration
  where
  toQuery DescribeOrganizationConfiguration' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { -- | Indicates whether GuardDuty is automatically enabled for accounts added
    -- to the organization.
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
    -- | Describes which data sources are enabled automatically for member
    -- accounts.
    dataSources :: Prelude.Maybe OrganizationDataSourceConfigurationsResult,
    -- | A list of features that are configured for this organization.
    features :: Prelude.Maybe [OrganizationFeatureConfigurationResult],
    -- | The pagination parameter to be used on the next list operation to
    -- retrieve more items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Indicates whether the maximum number of allowed member accounts are
    -- already associated with the delegated administrator account for your
    -- organization.
    memberAccountLimitReached :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'describeOrganizationConfigurationResponse_autoEnable' - Indicates whether GuardDuty is automatically enabled for accounts added
-- to the organization.
--
-- Even though this is still supported, we recommend using
-- @AutoEnableOrganizationMembers@ to achieve the similar results.
--
-- 'autoEnableOrganizationMembers', 'describeOrganizationConfigurationResponse_autoEnableOrganizationMembers' - Indicates the auto-enablement configuration of GuardDuty for the member
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
-- 'dataSources', 'describeOrganizationConfigurationResponse_dataSources' - Describes which data sources are enabled automatically for member
-- accounts.
--
-- 'features', 'describeOrganizationConfigurationResponse_features' - A list of features that are configured for this organization.
--
-- 'nextToken', 'describeOrganizationConfigurationResponse_nextToken' - The pagination parameter to be used on the next list operation to
-- retrieve more items.
--
-- 'httpStatus', 'describeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'memberAccountLimitReached', 'describeOrganizationConfigurationResponse_memberAccountLimitReached' - Indicates whether the maximum number of allowed member accounts are
-- already associated with the delegated administrator account for your
-- organization.
newDescribeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'memberAccountLimitReached'
  Prelude.Bool ->
  DescribeOrganizationConfigurationResponse
newDescribeOrganizationConfigurationResponse
  pHttpStatus_
  pMemberAccountLimitReached_ =
    DescribeOrganizationConfigurationResponse'
      { autoEnable =
          Prelude.Nothing,
        autoEnableOrganizationMembers =
          Prelude.Nothing,
        dataSources = Prelude.Nothing,
        features = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        memberAccountLimitReached =
          pMemberAccountLimitReached_
      }

-- | Indicates whether GuardDuty is automatically enabled for accounts added
-- to the organization.
--
-- Even though this is still supported, we recommend using
-- @AutoEnableOrganizationMembers@ to achieve the similar results.
describeOrganizationConfigurationResponse_autoEnable :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe Prelude.Bool)
describeOrganizationConfigurationResponse_autoEnable = Lens.lens (\DescribeOrganizationConfigurationResponse' {autoEnable} -> autoEnable) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {autoEnable = a} :: DescribeOrganizationConfigurationResponse)

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
describeOrganizationConfigurationResponse_autoEnableOrganizationMembers :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe AutoEnableMembers)
describeOrganizationConfigurationResponse_autoEnableOrganizationMembers = Lens.lens (\DescribeOrganizationConfigurationResponse' {autoEnableOrganizationMembers} -> autoEnableOrganizationMembers) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {autoEnableOrganizationMembers = a} :: DescribeOrganizationConfigurationResponse)

-- | Describes which data sources are enabled automatically for member
-- accounts.
describeOrganizationConfigurationResponse_dataSources :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe OrganizationDataSourceConfigurationsResult)
describeOrganizationConfigurationResponse_dataSources = Lens.lens (\DescribeOrganizationConfigurationResponse' {dataSources} -> dataSources) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {dataSources = a} :: DescribeOrganizationConfigurationResponse)

-- | A list of features that are configured for this organization.
describeOrganizationConfigurationResponse_features :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe [OrganizationFeatureConfigurationResult])
describeOrganizationConfigurationResponse_features = Lens.lens (\DescribeOrganizationConfigurationResponse' {features} -> features) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {features = a} :: DescribeOrganizationConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination parameter to be used on the next list operation to
-- retrieve more items.
describeOrganizationConfigurationResponse_nextToken :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe Prelude.Text)
describeOrganizationConfigurationResponse_nextToken = Lens.lens (\DescribeOrganizationConfigurationResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {nextToken = a} :: DescribeOrganizationConfigurationResponse)

-- | The response's http status code.
describeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Int
describeOrganizationConfigurationResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigurationResponse)

-- | Indicates whether the maximum number of allowed member accounts are
-- already associated with the delegated administrator account for your
-- organization.
describeOrganizationConfigurationResponse_memberAccountLimitReached :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Bool
describeOrganizationConfigurationResponse_memberAccountLimitReached = Lens.lens (\DescribeOrganizationConfigurationResponse' {memberAccountLimitReached} -> memberAccountLimitReached) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {memberAccountLimitReached = a} :: DescribeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigurationResponse
  where
  rnf DescribeOrganizationConfigurationResponse' {..} =
    Prelude.rnf autoEnable
      `Prelude.seq` Prelude.rnf autoEnableOrganizationMembers
      `Prelude.seq` Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf memberAccountLimitReached
