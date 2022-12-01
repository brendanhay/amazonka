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
-- Module      : Amazonka.KinesisAnalyticsV2.AddApplicationVpcConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a Virtual Private Cloud (VPC) configuration to the application.
-- Applications can use VPCs to store and access resources securely.
--
-- Note the following about VPC configurations for Kinesis Data Analytics
-- applications:
--
-- -   VPC configurations are not supported for SQL applications.
--
-- -   When a VPC is added to a Kinesis Data Analytics application, the
--     application can no longer be accessed from the Internet directly. To
--     enable Internet access to the application, add an Internet gateway
--     to your VPC.
module Amazonka.KinesisAnalyticsV2.AddApplicationVpcConfiguration
  ( -- * Creating a Request
    AddApplicationVpcConfiguration (..),
    newAddApplicationVpcConfiguration,

    -- * Request Lenses
    addApplicationVpcConfiguration_conditionalToken,
    addApplicationVpcConfiguration_currentApplicationVersionId,
    addApplicationVpcConfiguration_applicationName,
    addApplicationVpcConfiguration_vpcConfiguration,

    -- * Destructuring the Response
    AddApplicationVpcConfigurationResponse (..),
    newAddApplicationVpcConfigurationResponse,

    -- * Response Lenses
    addApplicationVpcConfigurationResponse_applicationARN,
    addApplicationVpcConfigurationResponse_applicationVersionId,
    addApplicationVpcConfigurationResponse_vpcConfigurationDescription,
    addApplicationVpcConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddApplicationVpcConfiguration' smart constructor.
data AddApplicationVpcConfiguration = AddApplicationVpcConfiguration'
  { -- | A value you use to implement strong concurrency for application updates.
    -- You must provide the @ApplicationVersionID@ or the @ConditionalToken@.
    -- You get the application\'s current @ConditionalToken@ using
    -- DescribeApplication. For better concurrency support, use the
    -- @ConditionalToken@ parameter instead of @CurrentApplicationVersionId@.
    conditionalToken :: Prelude.Maybe Prelude.Text,
    -- | The version of the application to which you want to add the VPC
    -- configuration. You must provide the @CurrentApplicationVersionId@ or the
    -- @ConditionalToken@. You can use the DescribeApplication operation to get
    -- the current application version. If the version specified is not the
    -- current version, the @ConcurrentModificationException@ is returned. For
    -- better concurrency support, use the @ConditionalToken@ parameter instead
    -- of @CurrentApplicationVersionId@.
    currentApplicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The name of an existing application.
    applicationName :: Prelude.Text,
    -- | Description of the VPC to add to the application.
    vpcConfiguration :: VpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationVpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalToken', 'addApplicationVpcConfiguration_conditionalToken' - A value you use to implement strong concurrency for application updates.
-- You must provide the @ApplicationVersionID@ or the @ConditionalToken@.
-- You get the application\'s current @ConditionalToken@ using
-- DescribeApplication. For better concurrency support, use the
-- @ConditionalToken@ parameter instead of @CurrentApplicationVersionId@.
--
-- 'currentApplicationVersionId', 'addApplicationVpcConfiguration_currentApplicationVersionId' - The version of the application to which you want to add the VPC
-- configuration. You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You can use the DescribeApplication operation to get
-- the current application version. If the version specified is not the
-- current version, the @ConcurrentModificationException@ is returned. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
--
-- 'applicationName', 'addApplicationVpcConfiguration_applicationName' - The name of an existing application.
--
-- 'vpcConfiguration', 'addApplicationVpcConfiguration_vpcConfiguration' - Description of the VPC to add to the application.
newAddApplicationVpcConfiguration ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'vpcConfiguration'
  VpcConfiguration ->
  AddApplicationVpcConfiguration
newAddApplicationVpcConfiguration
  pApplicationName_
  pVpcConfiguration_ =
    AddApplicationVpcConfiguration'
      { conditionalToken =
          Prelude.Nothing,
        currentApplicationVersionId =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        vpcConfiguration = pVpcConfiguration_
      }

-- | A value you use to implement strong concurrency for application updates.
-- You must provide the @ApplicationVersionID@ or the @ConditionalToken@.
-- You get the application\'s current @ConditionalToken@ using
-- DescribeApplication. For better concurrency support, use the
-- @ConditionalToken@ parameter instead of @CurrentApplicationVersionId@.
addApplicationVpcConfiguration_conditionalToken :: Lens.Lens' AddApplicationVpcConfiguration (Prelude.Maybe Prelude.Text)
addApplicationVpcConfiguration_conditionalToken = Lens.lens (\AddApplicationVpcConfiguration' {conditionalToken} -> conditionalToken) (\s@AddApplicationVpcConfiguration' {} a -> s {conditionalToken = a} :: AddApplicationVpcConfiguration)

-- | The version of the application to which you want to add the VPC
-- configuration. You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You can use the DescribeApplication operation to get
-- the current application version. If the version specified is not the
-- current version, the @ConcurrentModificationException@ is returned. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
addApplicationVpcConfiguration_currentApplicationVersionId :: Lens.Lens' AddApplicationVpcConfiguration (Prelude.Maybe Prelude.Natural)
addApplicationVpcConfiguration_currentApplicationVersionId = Lens.lens (\AddApplicationVpcConfiguration' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@AddApplicationVpcConfiguration' {} a -> s {currentApplicationVersionId = a} :: AddApplicationVpcConfiguration)

-- | The name of an existing application.
addApplicationVpcConfiguration_applicationName :: Lens.Lens' AddApplicationVpcConfiguration Prelude.Text
addApplicationVpcConfiguration_applicationName = Lens.lens (\AddApplicationVpcConfiguration' {applicationName} -> applicationName) (\s@AddApplicationVpcConfiguration' {} a -> s {applicationName = a} :: AddApplicationVpcConfiguration)

-- | Description of the VPC to add to the application.
addApplicationVpcConfiguration_vpcConfiguration :: Lens.Lens' AddApplicationVpcConfiguration VpcConfiguration
addApplicationVpcConfiguration_vpcConfiguration = Lens.lens (\AddApplicationVpcConfiguration' {vpcConfiguration} -> vpcConfiguration) (\s@AddApplicationVpcConfiguration' {} a -> s {vpcConfiguration = a} :: AddApplicationVpcConfiguration)

instance
  Core.AWSRequest
    AddApplicationVpcConfiguration
  where
  type
    AWSResponse AddApplicationVpcConfiguration =
      AddApplicationVpcConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddApplicationVpcConfigurationResponse'
            Prelude.<$> (x Core..?> "ApplicationARN")
            Prelude.<*> (x Core..?> "ApplicationVersionId")
            Prelude.<*> (x Core..?> "VpcConfigurationDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddApplicationVpcConfiguration
  where
  hashWithSalt
    _salt
    AddApplicationVpcConfiguration' {..} =
      _salt `Prelude.hashWithSalt` conditionalToken
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` vpcConfiguration

instance
  Prelude.NFData
    AddApplicationVpcConfiguration
  where
  rnf AddApplicationVpcConfiguration' {..} =
    Prelude.rnf conditionalToken
      `Prelude.seq` Prelude.rnf currentApplicationVersionId
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf vpcConfiguration

instance
  Core.ToHeaders
    AddApplicationVpcConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.AddApplicationVpcConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AddApplicationVpcConfiguration where
  toJSON AddApplicationVpcConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConditionalToken" Core..=)
              Prelude.<$> conditionalToken,
            ("CurrentApplicationVersionId" Core..=)
              Prelude.<$> currentApplicationVersionId,
            Prelude.Just
              ("ApplicationName" Core..= applicationName),
            Prelude.Just
              ("VpcConfiguration" Core..= vpcConfiguration)
          ]
      )

instance Core.ToPath AddApplicationVpcConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery AddApplicationVpcConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddApplicationVpcConfigurationResponse' smart constructor.
data AddApplicationVpcConfigurationResponse = AddApplicationVpcConfigurationResponse'
  { -- | The ARN of the application.
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | Provides the current application version. Kinesis Data Analytics updates
    -- the ApplicationVersionId each time you update the application.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The parameters of the new VPC configuration.
    vpcConfigurationDescription :: Prelude.Maybe VpcConfigurationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddApplicationVpcConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'addApplicationVpcConfigurationResponse_applicationARN' - The ARN of the application.
--
-- 'applicationVersionId', 'addApplicationVpcConfigurationResponse_applicationVersionId' - Provides the current application version. Kinesis Data Analytics updates
-- the ApplicationVersionId each time you update the application.
--
-- 'vpcConfigurationDescription', 'addApplicationVpcConfigurationResponse_vpcConfigurationDescription' - The parameters of the new VPC configuration.
--
-- 'httpStatus', 'addApplicationVpcConfigurationResponse_httpStatus' - The response's http status code.
newAddApplicationVpcConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddApplicationVpcConfigurationResponse
newAddApplicationVpcConfigurationResponse
  pHttpStatus_ =
    AddApplicationVpcConfigurationResponse'
      { applicationARN =
          Prelude.Nothing,
        applicationVersionId =
          Prelude.Nothing,
        vpcConfigurationDescription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the application.
addApplicationVpcConfigurationResponse_applicationARN :: Lens.Lens' AddApplicationVpcConfigurationResponse (Prelude.Maybe Prelude.Text)
addApplicationVpcConfigurationResponse_applicationARN = Lens.lens (\AddApplicationVpcConfigurationResponse' {applicationARN} -> applicationARN) (\s@AddApplicationVpcConfigurationResponse' {} a -> s {applicationARN = a} :: AddApplicationVpcConfigurationResponse)

-- | Provides the current application version. Kinesis Data Analytics updates
-- the ApplicationVersionId each time you update the application.
addApplicationVpcConfigurationResponse_applicationVersionId :: Lens.Lens' AddApplicationVpcConfigurationResponse (Prelude.Maybe Prelude.Natural)
addApplicationVpcConfigurationResponse_applicationVersionId = Lens.lens (\AddApplicationVpcConfigurationResponse' {applicationVersionId} -> applicationVersionId) (\s@AddApplicationVpcConfigurationResponse' {} a -> s {applicationVersionId = a} :: AddApplicationVpcConfigurationResponse)

-- | The parameters of the new VPC configuration.
addApplicationVpcConfigurationResponse_vpcConfigurationDescription :: Lens.Lens' AddApplicationVpcConfigurationResponse (Prelude.Maybe VpcConfigurationDescription)
addApplicationVpcConfigurationResponse_vpcConfigurationDescription = Lens.lens (\AddApplicationVpcConfigurationResponse' {vpcConfigurationDescription} -> vpcConfigurationDescription) (\s@AddApplicationVpcConfigurationResponse' {} a -> s {vpcConfigurationDescription = a} :: AddApplicationVpcConfigurationResponse)

-- | The response's http status code.
addApplicationVpcConfigurationResponse_httpStatus :: Lens.Lens' AddApplicationVpcConfigurationResponse Prelude.Int
addApplicationVpcConfigurationResponse_httpStatus = Lens.lens (\AddApplicationVpcConfigurationResponse' {httpStatus} -> httpStatus) (\s@AddApplicationVpcConfigurationResponse' {} a -> s {httpStatus = a} :: AddApplicationVpcConfigurationResponse)

instance
  Prelude.NFData
    AddApplicationVpcConfigurationResponse
  where
  rnf AddApplicationVpcConfigurationResponse' {..} =
    Prelude.rnf applicationARN
      `Prelude.seq` Prelude.rnf applicationVersionId
      `Prelude.seq` Prelude.rnf vpcConfigurationDescription
      `Prelude.seq` Prelude.rnf httpStatus
