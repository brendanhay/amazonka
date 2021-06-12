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
-- Module      : Network.AWS.Config.PutRetentionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the retention configuration with details about
-- retention period (number of days) that AWS Config stores your historical
-- information. The API creates the @RetentionConfiguration@ object and
-- names the object as __default__. When you have a
-- @RetentionConfiguration@ object named __default__, calling the API
-- modifies the default object.
--
-- Currently, AWS Config supports only one retention configuration per
-- region in your account.
module Network.AWS.Config.PutRetentionConfiguration
  ( -- * Creating a Request
    PutRetentionConfiguration (..),
    newPutRetentionConfiguration,

    -- * Request Lenses
    putRetentionConfiguration_retentionPeriodInDays,

    -- * Destructuring the Response
    PutRetentionConfigurationResponse (..),
    newPutRetentionConfigurationResponse,

    -- * Response Lenses
    putRetentionConfigurationResponse_retentionConfiguration,
    putRetentionConfigurationResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRetentionConfiguration' smart constructor.
data PutRetentionConfiguration = PutRetentionConfiguration'
  { -- | Number of days AWS Config stores your historical information.
    --
    -- Currently, only applicable to the configuration item history.
    retentionPeriodInDays :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRetentionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriodInDays', 'putRetentionConfiguration_retentionPeriodInDays' - Number of days AWS Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
newPutRetentionConfiguration ::
  -- | 'retentionPeriodInDays'
  Core.Natural ->
  PutRetentionConfiguration
newPutRetentionConfiguration pRetentionPeriodInDays_ =
  PutRetentionConfiguration'
    { retentionPeriodInDays =
        pRetentionPeriodInDays_
    }

-- | Number of days AWS Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
putRetentionConfiguration_retentionPeriodInDays :: Lens.Lens' PutRetentionConfiguration Core.Natural
putRetentionConfiguration_retentionPeriodInDays = Lens.lens (\PutRetentionConfiguration' {retentionPeriodInDays} -> retentionPeriodInDays) (\s@PutRetentionConfiguration' {} a -> s {retentionPeriodInDays = a} :: PutRetentionConfiguration)

instance Core.AWSRequest PutRetentionConfiguration where
  type
    AWSResponse PutRetentionConfiguration =
      PutRetentionConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRetentionConfigurationResponse'
            Core.<$> (x Core..?> "RetentionConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRetentionConfiguration

instance Core.NFData PutRetentionConfiguration

instance Core.ToHeaders PutRetentionConfiguration where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.PutRetentionConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRetentionConfiguration where
  toJSON PutRetentionConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "RetentionPeriodInDays"
                  Core..= retentionPeriodInDays
              )
          ]
      )

instance Core.ToPath PutRetentionConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery PutRetentionConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRetentionConfigurationResponse' smart constructor.
data PutRetentionConfigurationResponse = PutRetentionConfigurationResponse'
  { -- | Returns a retention configuration object.
    retentionConfiguration :: Core.Maybe RetentionConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutRetentionConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionConfiguration', 'putRetentionConfigurationResponse_retentionConfiguration' - Returns a retention configuration object.
--
-- 'httpStatus', 'putRetentionConfigurationResponse_httpStatus' - The response's http status code.
newPutRetentionConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutRetentionConfigurationResponse
newPutRetentionConfigurationResponse pHttpStatus_ =
  PutRetentionConfigurationResponse'
    { retentionConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a retention configuration object.
putRetentionConfigurationResponse_retentionConfiguration :: Lens.Lens' PutRetentionConfigurationResponse (Core.Maybe RetentionConfiguration)
putRetentionConfigurationResponse_retentionConfiguration = Lens.lens (\PutRetentionConfigurationResponse' {retentionConfiguration} -> retentionConfiguration) (\s@PutRetentionConfigurationResponse' {} a -> s {retentionConfiguration = a} :: PutRetentionConfigurationResponse)

-- | The response's http status code.
putRetentionConfigurationResponse_httpStatus :: Lens.Lens' PutRetentionConfigurationResponse Core.Int
putRetentionConfigurationResponse_httpStatus = Lens.lens (\PutRetentionConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutRetentionConfigurationResponse' {} a -> s {httpStatus = a} :: PutRetentionConfigurationResponse)

instance
  Core.NFData
    PutRetentionConfigurationResponse
