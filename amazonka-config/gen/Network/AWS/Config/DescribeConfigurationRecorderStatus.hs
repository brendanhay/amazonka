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
-- Module      : Network.AWS.Config.DescribeConfigurationRecorderStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified configuration recorder. If a
-- configuration recorder is not specified, this action returns the status
-- of all configuration recorders associated with the account.
--
-- Currently, you can specify only one configuration recorder per region in
-- your account.
module Network.AWS.Config.DescribeConfigurationRecorderStatus
  ( -- * Creating a Request
    DescribeConfigurationRecorderStatus (..),
    newDescribeConfigurationRecorderStatus,

    -- * Request Lenses
    describeConfigurationRecorderStatus_configurationRecorderNames,

    -- * Destructuring the Response
    DescribeConfigurationRecorderStatusResponse (..),
    newDescribeConfigurationRecorderStatusResponse,

    -- * Response Lenses
    describeConfigurationRecorderStatusResponse_configurationRecordersStatus,
    describeConfigurationRecorderStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeConfigurationRecorderStatus action.
--
-- /See:/ 'newDescribeConfigurationRecorderStatus' smart constructor.
data DescribeConfigurationRecorderStatus = DescribeConfigurationRecorderStatus'
  { -- | The name(s) of the configuration recorder. If the name is not specified,
    -- the action returns the current status of all the configuration recorders
    -- associated with the account.
    configurationRecorderNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationRecorderStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorderNames', 'describeConfigurationRecorderStatus_configurationRecorderNames' - The name(s) of the configuration recorder. If the name is not specified,
-- the action returns the current status of all the configuration recorders
-- associated with the account.
newDescribeConfigurationRecorderStatus ::
  DescribeConfigurationRecorderStatus
newDescribeConfigurationRecorderStatus =
  DescribeConfigurationRecorderStatus'
    { configurationRecorderNames =
        Core.Nothing
    }

-- | The name(s) of the configuration recorder. If the name is not specified,
-- the action returns the current status of all the configuration recorders
-- associated with the account.
describeConfigurationRecorderStatus_configurationRecorderNames :: Lens.Lens' DescribeConfigurationRecorderStatus (Core.Maybe [Core.Text])
describeConfigurationRecorderStatus_configurationRecorderNames = Lens.lens (\DescribeConfigurationRecorderStatus' {configurationRecorderNames} -> configurationRecorderNames) (\s@DescribeConfigurationRecorderStatus' {} a -> s {configurationRecorderNames = a} :: DescribeConfigurationRecorderStatus) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSRequest
    DescribeConfigurationRecorderStatus
  where
  type
    AWSResponse DescribeConfigurationRecorderStatus =
      DescribeConfigurationRecorderStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationRecorderStatusResponse'
            Core.<$> ( x Core..?> "ConfigurationRecordersStatus"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeConfigurationRecorderStatus

instance
  Core.NFData
    DescribeConfigurationRecorderStatus

instance
  Core.ToHeaders
    DescribeConfigurationRecorderStatus
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeConfigurationRecorderStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeConfigurationRecorderStatus
  where
  toJSON DescribeConfigurationRecorderStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConfigurationRecorderNames" Core..=)
              Core.<$> configurationRecorderNames
          ]
      )

instance
  Core.ToPath
    DescribeConfigurationRecorderStatus
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeConfigurationRecorderStatus
  where
  toQuery = Core.const Core.mempty

-- | The output for the DescribeConfigurationRecorderStatus action, in JSON
-- format.
--
-- /See:/ 'newDescribeConfigurationRecorderStatusResponse' smart constructor.
data DescribeConfigurationRecorderStatusResponse = DescribeConfigurationRecorderStatusResponse'
  { -- | A list that contains status of the specified recorders.
    configurationRecordersStatus :: Core.Maybe [ConfigurationRecorderStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeConfigurationRecorderStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecordersStatus', 'describeConfigurationRecorderStatusResponse_configurationRecordersStatus' - A list that contains status of the specified recorders.
--
-- 'httpStatus', 'describeConfigurationRecorderStatusResponse_httpStatus' - The response's http status code.
newDescribeConfigurationRecorderStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeConfigurationRecorderStatusResponse
newDescribeConfigurationRecorderStatusResponse
  pHttpStatus_ =
    DescribeConfigurationRecorderStatusResponse'
      { configurationRecordersStatus =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list that contains status of the specified recorders.
describeConfigurationRecorderStatusResponse_configurationRecordersStatus :: Lens.Lens' DescribeConfigurationRecorderStatusResponse (Core.Maybe [ConfigurationRecorderStatus])
describeConfigurationRecorderStatusResponse_configurationRecordersStatus = Lens.lens (\DescribeConfigurationRecorderStatusResponse' {configurationRecordersStatus} -> configurationRecordersStatus) (\s@DescribeConfigurationRecorderStatusResponse' {} a -> s {configurationRecordersStatus = a} :: DescribeConfigurationRecorderStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeConfigurationRecorderStatusResponse_httpStatus :: Lens.Lens' DescribeConfigurationRecorderStatusResponse Core.Int
describeConfigurationRecorderStatusResponse_httpStatus = Lens.lens (\DescribeConfigurationRecorderStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationRecorderStatusResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationRecorderStatusResponse)

instance
  Core.NFData
    DescribeConfigurationRecorderStatusResponse
