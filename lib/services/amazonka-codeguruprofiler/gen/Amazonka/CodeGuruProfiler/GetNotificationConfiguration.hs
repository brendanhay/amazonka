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
-- Module      : Amazonka.CodeGuruProfiler.GetNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the current configuration for anomaly notifications for a profiling
-- group.
module Amazonka.CodeGuruProfiler.GetNotificationConfiguration
  ( -- * Creating a Request
    GetNotificationConfiguration (..),
    newGetNotificationConfiguration,

    -- * Request Lenses
    getNotificationConfiguration_profilingGroupName,

    -- * Destructuring the Response
    GetNotificationConfigurationResponse (..),
    newGetNotificationConfigurationResponse,

    -- * Response Lenses
    getNotificationConfigurationResponse_httpStatus,
    getNotificationConfigurationResponse_notificationConfiguration,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the GetNotificationConfigurationRequest.
--
-- /See:/ 'newGetNotificationConfiguration' smart constructor.
data GetNotificationConfiguration = GetNotificationConfiguration'
  { -- | The name of the profiling group we want to get the notification
    -- configuration for.
    profilingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profilingGroupName', 'getNotificationConfiguration_profilingGroupName' - The name of the profiling group we want to get the notification
-- configuration for.
newGetNotificationConfiguration ::
  -- | 'profilingGroupName'
  Prelude.Text ->
  GetNotificationConfiguration
newGetNotificationConfiguration pProfilingGroupName_ =
  GetNotificationConfiguration'
    { profilingGroupName =
        pProfilingGroupName_
    }

-- | The name of the profiling group we want to get the notification
-- configuration for.
getNotificationConfiguration_profilingGroupName :: Lens.Lens' GetNotificationConfiguration Prelude.Text
getNotificationConfiguration_profilingGroupName = Lens.lens (\GetNotificationConfiguration' {profilingGroupName} -> profilingGroupName) (\s@GetNotificationConfiguration' {} a -> s {profilingGroupName = a} :: GetNotificationConfiguration)

instance Core.AWSRequest GetNotificationConfiguration where
  type
    AWSResponse GetNotificationConfiguration =
      GetNotificationConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNotificationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "notificationConfiguration")
      )

instance
  Prelude.Hashable
    GetNotificationConfiguration
  where
  hashWithSalt _salt GetNotificationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` profilingGroupName

instance Prelude.NFData GetNotificationConfiguration where
  rnf GetNotificationConfiguration' {..} =
    Prelude.rnf profilingGroupName

instance Data.ToHeaders GetNotificationConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNotificationConfiguration where
  toPath GetNotificationConfiguration' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Data.toBS profilingGroupName,
        "/notificationConfiguration"
      ]

instance Data.ToQuery GetNotificationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | The structure representing the GetNotificationConfigurationResponse.
--
-- /See:/ 'newGetNotificationConfigurationResponse' smart constructor.
data GetNotificationConfigurationResponse = GetNotificationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current notification configuration for this profiling group.
    notificationConfiguration :: NotificationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getNotificationConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'notificationConfiguration', 'getNotificationConfigurationResponse_notificationConfiguration' - The current notification configuration for this profiling group.
newGetNotificationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'notificationConfiguration'
  NotificationConfiguration ->
  GetNotificationConfigurationResponse
newGetNotificationConfigurationResponse
  pHttpStatus_
  pNotificationConfiguration_ =
    GetNotificationConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        notificationConfiguration =
          pNotificationConfiguration_
      }

-- | The response's http status code.
getNotificationConfigurationResponse_httpStatus :: Lens.Lens' GetNotificationConfigurationResponse Prelude.Int
getNotificationConfigurationResponse_httpStatus = Lens.lens (\GetNotificationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetNotificationConfigurationResponse' {} a -> s {httpStatus = a} :: GetNotificationConfigurationResponse)

-- | The current notification configuration for this profiling group.
getNotificationConfigurationResponse_notificationConfiguration :: Lens.Lens' GetNotificationConfigurationResponse NotificationConfiguration
getNotificationConfigurationResponse_notificationConfiguration = Lens.lens (\GetNotificationConfigurationResponse' {notificationConfiguration} -> notificationConfiguration) (\s@GetNotificationConfigurationResponse' {} a -> s {notificationConfiguration = a} :: GetNotificationConfigurationResponse)

instance
  Prelude.NFData
    GetNotificationConfigurationResponse
  where
  rnf GetNotificationConfigurationResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf notificationConfiguration
