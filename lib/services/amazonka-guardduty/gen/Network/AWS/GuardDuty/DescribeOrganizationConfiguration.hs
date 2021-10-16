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
-- Module      : Network.AWS.GuardDuty.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the account selected as the delegated
-- administrator for GuardDuty.
module Network.AWS.GuardDuty.DescribeOrganizationConfiguration
  ( -- * Creating a Request
    DescribeOrganizationConfiguration (..),
    newDescribeOrganizationConfiguration,

    -- * Request Lenses
    describeOrganizationConfiguration_detectorId,

    -- * Destructuring the Response
    DescribeOrganizationConfigurationResponse (..),
    newDescribeOrganizationConfigurationResponse,

    -- * Response Lenses
    describeOrganizationConfigurationResponse_dataSources,
    describeOrganizationConfigurationResponse_httpStatus,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeOrganizationConfiguration' smart constructor.
data DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  { -- | The ID of the detector to retrieve information about the delegated
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
-- 'detectorId', 'describeOrganizationConfiguration_detectorId' - The ID of the detector to retrieve information about the delegated
-- administrator from.
newDescribeOrganizationConfiguration ::
  -- | 'detectorId'
  Prelude.Text ->
  DescribeOrganizationConfiguration
newDescribeOrganizationConfiguration pDetectorId_ =
  DescribeOrganizationConfiguration'
    { detectorId =
        pDetectorId_
    }

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigurationResponse'
            Prelude.<$> (x Core..?> "dataSources")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "autoEnable")
              Prelude.<*> (x Core..:> "memberAccountLimitReached")
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfiguration

instance
  Prelude.NFData
    DescribeOrganizationConfiguration

instance
  Core.ToHeaders
    DescribeOrganizationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    DescribeOrganizationConfiguration
  where
  toPath DescribeOrganizationConfiguration' {..} =
    Prelude.mconcat
      ["/detector/", Core.toBS detectorId, "/admin"]

instance
  Core.ToQuery
    DescribeOrganizationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { -- | Describes which data sources are enabled automatically for member
    -- accounts.
    dataSources :: Prelude.Maybe OrganizationDataSourceConfigurationsResult,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Indicates whether GuardDuty is automatically enabled for accounts added
    -- to the organization.
    autoEnable :: Prelude.Bool,
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
-- 'dataSources', 'describeOrganizationConfigurationResponse_dataSources' - Describes which data sources are enabled automatically for member
-- accounts.
--
-- 'httpStatus', 'describeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'autoEnable', 'describeOrganizationConfigurationResponse_autoEnable' - Indicates whether GuardDuty is automatically enabled for accounts added
-- to the organization.
--
-- 'memberAccountLimitReached', 'describeOrganizationConfigurationResponse_memberAccountLimitReached' - Indicates whether the maximum number of allowed member accounts are
-- already associated with the delegated administrator account for your
-- organization.
newDescribeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'autoEnable'
  Prelude.Bool ->
  -- | 'memberAccountLimitReached'
  Prelude.Bool ->
  DescribeOrganizationConfigurationResponse
newDescribeOrganizationConfigurationResponse
  pHttpStatus_
  pAutoEnable_
  pMemberAccountLimitReached_ =
    DescribeOrganizationConfigurationResponse'
      { dataSources =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        autoEnable = pAutoEnable_,
        memberAccountLimitReached =
          pMemberAccountLimitReached_
      }

-- | Describes which data sources are enabled automatically for member
-- accounts.
describeOrganizationConfigurationResponse_dataSources :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe OrganizationDataSourceConfigurationsResult)
describeOrganizationConfigurationResponse_dataSources = Lens.lens (\DescribeOrganizationConfigurationResponse' {dataSources} -> dataSources) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {dataSources = a} :: DescribeOrganizationConfigurationResponse)

-- | The response's http status code.
describeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Int
describeOrganizationConfigurationResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigurationResponse)

-- | Indicates whether GuardDuty is automatically enabled for accounts added
-- to the organization.
describeOrganizationConfigurationResponse_autoEnable :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Bool
describeOrganizationConfigurationResponse_autoEnable = Lens.lens (\DescribeOrganizationConfigurationResponse' {autoEnable} -> autoEnable) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {autoEnable = a} :: DescribeOrganizationConfigurationResponse)

-- | Indicates whether the maximum number of allowed member accounts are
-- already associated with the delegated administrator account for your
-- organization.
describeOrganizationConfigurationResponse_memberAccountLimitReached :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Bool
describeOrganizationConfigurationResponse_memberAccountLimitReached = Lens.lens (\DescribeOrganizationConfigurationResponse' {memberAccountLimitReached} -> memberAccountLimitReached) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {memberAccountLimitReached = a} :: DescribeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigurationResponse
