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
-- Module      : Amazonka.WAFV2.PutLoggingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified LoggingConfiguration, to start logging from a web
-- ACL, according to the configuration provided.
--
-- You can define one logging destination per web ACL.
--
-- You can access information about the traffic that WAF inspects using the
-- following steps:
--
-- 1.  Create your logging destination. You can use an Amazon CloudWatch
--     Logs log group, an Amazon Simple Storage Service (Amazon S3) bucket,
--     or an Amazon Kinesis Data Firehose.
--
--     The name that you give the destination must start with
--     @aws-waf-logs-@. Depending on the type of destination, you might
--     need to configure additional settings or permissions.
--
--     For configuration requirements and pricing information for each
--     destination type, see
--     <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging web ACL traffic>
--     in the /WAF Developer Guide/.
--
-- 2.  Associate your logging destination to your web ACL using a
--     @PutLoggingConfiguration@ request.
--
-- When you successfully enable logging using a @PutLoggingConfiguration@
-- request, WAF creates an additional role or policy that is required to
-- write logs to the logging destination. For an Amazon CloudWatch Logs log
-- group, WAF creates a resource policy on the log group. For an Amazon S3
-- bucket, WAF creates a bucket policy. For an Amazon Kinesis Data
-- Firehose, WAF creates a service-linked role.
--
-- For additional information about web ACL logging, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging web ACL traffic information>
-- in the /WAF Developer Guide/.
--
-- This operation completely replaces the mutable specifications that you
-- already have for the logging configuration with the ones that you
-- provide to this call. To modify the logging configuration, retrieve it
-- by calling GetLoggingConfiguration, update the settings as needed, and
-- then provide the complete logging configuration specification to this
-- call.
module Amazonka.WAFV2.PutLoggingConfiguration
  ( -- * Creating a Request
    PutLoggingConfiguration (..),
    newPutLoggingConfiguration,

    -- * Request Lenses
    putLoggingConfiguration_loggingConfiguration,

    -- * Destructuring the Response
    PutLoggingConfigurationResponse (..),
    newPutLoggingConfigurationResponse,

    -- * Response Lenses
    putLoggingConfigurationResponse_loggingConfiguration,
    putLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newPutLoggingConfiguration' smart constructor.
data PutLoggingConfiguration = PutLoggingConfiguration'
  { loggingConfiguration :: LoggingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'putLoggingConfiguration_loggingConfiguration' -
newPutLoggingConfiguration ::
  -- | 'loggingConfiguration'
  LoggingConfiguration ->
  PutLoggingConfiguration
newPutLoggingConfiguration pLoggingConfiguration_ =
  PutLoggingConfiguration'
    { loggingConfiguration =
        pLoggingConfiguration_
    }

putLoggingConfiguration_loggingConfiguration :: Lens.Lens' PutLoggingConfiguration LoggingConfiguration
putLoggingConfiguration_loggingConfiguration = Lens.lens (\PutLoggingConfiguration' {loggingConfiguration} -> loggingConfiguration) (\s@PutLoggingConfiguration' {} a -> s {loggingConfiguration = a} :: PutLoggingConfiguration)

instance Core.AWSRequest PutLoggingConfiguration where
  type
    AWSResponse PutLoggingConfiguration =
      PutLoggingConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutLoggingConfigurationResponse'
            Prelude.<$> (x Data..?> "LoggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLoggingConfiguration where
  hashWithSalt _salt PutLoggingConfiguration' {..} =
    _salt `Prelude.hashWithSalt` loggingConfiguration

instance Prelude.NFData PutLoggingConfiguration where
  rnf PutLoggingConfiguration' {..} =
    Prelude.rnf loggingConfiguration

instance Data.ToHeaders PutLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.PutLoggingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutLoggingConfiguration where
  toJSON PutLoggingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "LoggingConfiguration"
                  Data..= loggingConfiguration
              )
          ]
      )

instance Data.ToPath PutLoggingConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLoggingConfigurationResponse' smart constructor.
data PutLoggingConfigurationResponse = PutLoggingConfigurationResponse'
  { loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'putLoggingConfigurationResponse_loggingConfiguration' -
--
-- 'httpStatus', 'putLoggingConfigurationResponse_httpStatus' - The response's http status code.
newPutLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutLoggingConfigurationResponse
newPutLoggingConfigurationResponse pHttpStatus_ =
  PutLoggingConfigurationResponse'
    { loggingConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

putLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' PutLoggingConfigurationResponse (Prelude.Maybe LoggingConfiguration)
putLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\PutLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@PutLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: PutLoggingConfigurationResponse)

-- | The response's http status code.
putLoggingConfigurationResponse_httpStatus :: Lens.Lens' PutLoggingConfigurationResponse Prelude.Int
putLoggingConfigurationResponse_httpStatus = Lens.lens (\PutLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: PutLoggingConfigurationResponse)

instance
  Prelude.NFData
    PutLoggingConfigurationResponse
  where
  rnf PutLoggingConfigurationResponse' {..} =
    Prelude.rnf loggingConfiguration `Prelude.seq`
      Prelude.rnf httpStatus
