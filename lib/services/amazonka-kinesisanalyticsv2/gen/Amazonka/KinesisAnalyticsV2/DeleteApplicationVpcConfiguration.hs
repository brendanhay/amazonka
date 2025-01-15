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
-- Module      : Amazonka.KinesisAnalyticsV2.DeleteApplicationVpcConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a VPC configuration from a Kinesis Data Analytics application.
module Amazonka.KinesisAnalyticsV2.DeleteApplicationVpcConfiguration
  ( -- * Creating a Request
    DeleteApplicationVpcConfiguration (..),
    newDeleteApplicationVpcConfiguration,

    -- * Request Lenses
    deleteApplicationVpcConfiguration_conditionalToken,
    deleteApplicationVpcConfiguration_currentApplicationVersionId,
    deleteApplicationVpcConfiguration_applicationName,
    deleteApplicationVpcConfiguration_vpcConfigurationId,

    -- * Destructuring the Response
    DeleteApplicationVpcConfigurationResponse (..),
    newDeleteApplicationVpcConfigurationResponse,

    -- * Response Lenses
    deleteApplicationVpcConfigurationResponse_applicationARN,
    deleteApplicationVpcConfigurationResponse_applicationVersionId,
    deleteApplicationVpcConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteApplicationVpcConfiguration' smart constructor.
data DeleteApplicationVpcConfiguration = DeleteApplicationVpcConfiguration'
  { -- | A value you use to implement strong concurrency for application updates.
    -- You must provide the @CurrentApplicationVersionId@ or the
    -- @ConditionalToken@. You get the application\'s current
    -- @ConditionalToken@ using DescribeApplication. For better concurrency
    -- support, use the @ConditionalToken@ parameter instead of
    -- @CurrentApplicationVersionId@.
    conditionalToken :: Prelude.Maybe Prelude.Text,
    -- | The current application version ID. You must provide the
    -- @CurrentApplicationVersionId@ or the @ConditionalToken@. You can
    -- retrieve the application version ID using DescribeApplication. For
    -- better concurrency support, use the @ConditionalToken@ parameter instead
    -- of @CurrentApplicationVersionId@.
    currentApplicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The name of an existing application.
    applicationName :: Prelude.Text,
    -- | The ID of the VPC configuration to delete.
    vpcConfigurationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationVpcConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalToken', 'deleteApplicationVpcConfiguration_conditionalToken' - A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
--
-- 'currentApplicationVersionId', 'deleteApplicationVpcConfiguration_currentApplicationVersionId' - The current application version ID. You must provide the
-- @CurrentApplicationVersionId@ or the @ConditionalToken@. You can
-- retrieve the application version ID using DescribeApplication. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
--
-- 'applicationName', 'deleteApplicationVpcConfiguration_applicationName' - The name of an existing application.
--
-- 'vpcConfigurationId', 'deleteApplicationVpcConfiguration_vpcConfigurationId' - The ID of the VPC configuration to delete.
newDeleteApplicationVpcConfiguration ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'vpcConfigurationId'
  Prelude.Text ->
  DeleteApplicationVpcConfiguration
newDeleteApplicationVpcConfiguration
  pApplicationName_
  pVpcConfigurationId_ =
    DeleteApplicationVpcConfiguration'
      { conditionalToken =
          Prelude.Nothing,
        currentApplicationVersionId =
          Prelude.Nothing,
        applicationName = pApplicationName_,
        vpcConfigurationId =
          pVpcConfigurationId_
      }

-- | A value you use to implement strong concurrency for application updates.
-- You must provide the @CurrentApplicationVersionId@ or the
-- @ConditionalToken@. You get the application\'s current
-- @ConditionalToken@ using DescribeApplication. For better concurrency
-- support, use the @ConditionalToken@ parameter instead of
-- @CurrentApplicationVersionId@.
deleteApplicationVpcConfiguration_conditionalToken :: Lens.Lens' DeleteApplicationVpcConfiguration (Prelude.Maybe Prelude.Text)
deleteApplicationVpcConfiguration_conditionalToken = Lens.lens (\DeleteApplicationVpcConfiguration' {conditionalToken} -> conditionalToken) (\s@DeleteApplicationVpcConfiguration' {} a -> s {conditionalToken = a} :: DeleteApplicationVpcConfiguration)

-- | The current application version ID. You must provide the
-- @CurrentApplicationVersionId@ or the @ConditionalToken@. You can
-- retrieve the application version ID using DescribeApplication. For
-- better concurrency support, use the @ConditionalToken@ parameter instead
-- of @CurrentApplicationVersionId@.
deleteApplicationVpcConfiguration_currentApplicationVersionId :: Lens.Lens' DeleteApplicationVpcConfiguration (Prelude.Maybe Prelude.Natural)
deleteApplicationVpcConfiguration_currentApplicationVersionId = Lens.lens (\DeleteApplicationVpcConfiguration' {currentApplicationVersionId} -> currentApplicationVersionId) (\s@DeleteApplicationVpcConfiguration' {} a -> s {currentApplicationVersionId = a} :: DeleteApplicationVpcConfiguration)

-- | The name of an existing application.
deleteApplicationVpcConfiguration_applicationName :: Lens.Lens' DeleteApplicationVpcConfiguration Prelude.Text
deleteApplicationVpcConfiguration_applicationName = Lens.lens (\DeleteApplicationVpcConfiguration' {applicationName} -> applicationName) (\s@DeleteApplicationVpcConfiguration' {} a -> s {applicationName = a} :: DeleteApplicationVpcConfiguration)

-- | The ID of the VPC configuration to delete.
deleteApplicationVpcConfiguration_vpcConfigurationId :: Lens.Lens' DeleteApplicationVpcConfiguration Prelude.Text
deleteApplicationVpcConfiguration_vpcConfigurationId = Lens.lens (\DeleteApplicationVpcConfiguration' {vpcConfigurationId} -> vpcConfigurationId) (\s@DeleteApplicationVpcConfiguration' {} a -> s {vpcConfigurationId = a} :: DeleteApplicationVpcConfiguration)

instance
  Core.AWSRequest
    DeleteApplicationVpcConfiguration
  where
  type
    AWSResponse DeleteApplicationVpcConfiguration =
      DeleteApplicationVpcConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteApplicationVpcConfigurationResponse'
            Prelude.<$> (x Data..?> "ApplicationARN")
            Prelude.<*> (x Data..?> "ApplicationVersionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteApplicationVpcConfiguration
  where
  hashWithSalt
    _salt
    DeleteApplicationVpcConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` conditionalToken
        `Prelude.hashWithSalt` currentApplicationVersionId
        `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` vpcConfigurationId

instance
  Prelude.NFData
    DeleteApplicationVpcConfiguration
  where
  rnf DeleteApplicationVpcConfiguration' {..} =
    Prelude.rnf conditionalToken `Prelude.seq`
      Prelude.rnf currentApplicationVersionId `Prelude.seq`
        Prelude.rnf applicationName `Prelude.seq`
          Prelude.rnf vpcConfigurationId

instance
  Data.ToHeaders
    DeleteApplicationVpcConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "KinesisAnalytics_20180523.DeleteApplicationVpcConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DeleteApplicationVpcConfiguration
  where
  toJSON DeleteApplicationVpcConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalToken" Data..=)
              Prelude.<$> conditionalToken,
            ("CurrentApplicationVersionId" Data..=)
              Prelude.<$> currentApplicationVersionId,
            Prelude.Just
              ("ApplicationName" Data..= applicationName),
            Prelude.Just
              ("VpcConfigurationId" Data..= vpcConfigurationId)
          ]
      )

instance
  Data.ToPath
    DeleteApplicationVpcConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteApplicationVpcConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteApplicationVpcConfigurationResponse' smart constructor.
data DeleteApplicationVpcConfigurationResponse = DeleteApplicationVpcConfigurationResponse'
  { -- | The ARN of the Kinesis Data Analytics application.
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | The updated version ID of the application.
    applicationVersionId :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteApplicationVpcConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'deleteApplicationVpcConfigurationResponse_applicationARN' - The ARN of the Kinesis Data Analytics application.
--
-- 'applicationVersionId', 'deleteApplicationVpcConfigurationResponse_applicationVersionId' - The updated version ID of the application.
--
-- 'httpStatus', 'deleteApplicationVpcConfigurationResponse_httpStatus' - The response's http status code.
newDeleteApplicationVpcConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteApplicationVpcConfigurationResponse
newDeleteApplicationVpcConfigurationResponse
  pHttpStatus_ =
    DeleteApplicationVpcConfigurationResponse'
      { applicationARN =
          Prelude.Nothing,
        applicationVersionId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the Kinesis Data Analytics application.
deleteApplicationVpcConfigurationResponse_applicationARN :: Lens.Lens' DeleteApplicationVpcConfigurationResponse (Prelude.Maybe Prelude.Text)
deleteApplicationVpcConfigurationResponse_applicationARN = Lens.lens (\DeleteApplicationVpcConfigurationResponse' {applicationARN} -> applicationARN) (\s@DeleteApplicationVpcConfigurationResponse' {} a -> s {applicationARN = a} :: DeleteApplicationVpcConfigurationResponse)

-- | The updated version ID of the application.
deleteApplicationVpcConfigurationResponse_applicationVersionId :: Lens.Lens' DeleteApplicationVpcConfigurationResponse (Prelude.Maybe Prelude.Natural)
deleteApplicationVpcConfigurationResponse_applicationVersionId = Lens.lens (\DeleteApplicationVpcConfigurationResponse' {applicationVersionId} -> applicationVersionId) (\s@DeleteApplicationVpcConfigurationResponse' {} a -> s {applicationVersionId = a} :: DeleteApplicationVpcConfigurationResponse)

-- | The response's http status code.
deleteApplicationVpcConfigurationResponse_httpStatus :: Lens.Lens' DeleteApplicationVpcConfigurationResponse Prelude.Int
deleteApplicationVpcConfigurationResponse_httpStatus = Lens.lens (\DeleteApplicationVpcConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteApplicationVpcConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteApplicationVpcConfigurationResponse)

instance
  Prelude.NFData
    DeleteApplicationVpcConfigurationResponse
  where
  rnf DeleteApplicationVpcConfigurationResponse' {..} =
    Prelude.rnf applicationARN `Prelude.seq`
      Prelude.rnf applicationVersionId `Prelude.seq`
        Prelude.rnf httpStatus
