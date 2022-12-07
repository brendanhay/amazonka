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
-- Module      : Amazonka.WAF.PutLoggingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Associates a LoggingConfiguration with a specified web ACL.
--
-- You can access information about all traffic that AWS WAF inspects using
-- the following steps:
--
-- 1.  Create an Amazon Kinesis Data Firehose.
--
--     Create the data firehose with a PUT source and in the region that
--     you are operating. However, if you are capturing logs for Amazon
--     CloudFront, always create the firehose in US East (N. Virginia).
--
--     Do not create the data firehose using a @Kinesis stream@ as your
--     source.
--
-- 2.  Associate that firehose to your web ACL using a
--     @PutLoggingConfiguration@ request.
--
-- When you successfully enable logging using a @PutLoggingConfiguration@
-- request, AWS WAF will create a service linked role with the necessary
-- permissions to write logs to the Amazon Kinesis Data Firehose. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/logging.html Logging Web ACL Traffic Information>
-- in the /AWS WAF Developer Guide/.
module Amazonka.WAF.PutLoggingConfiguration
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
import Amazonka.WAF.Types

-- | /See:/ 'newPutLoggingConfiguration' smart constructor.
data PutLoggingConfiguration = PutLoggingConfiguration'
  { -- | The Amazon Kinesis Data Firehose that contains the inspected traffic
    -- information, the redacted fields details, and the Amazon Resource Name
    -- (ARN) of the web ACL to monitor.
    --
    -- When specifying @Type@ in @RedactedFields@, you must use one of the
    -- following values: @URI@, @QUERY_STRING@, @HEADER@, or @METHOD@.
    loggingConfiguration :: LoggingConfiguration
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
-- 'loggingConfiguration', 'putLoggingConfiguration_loggingConfiguration' - The Amazon Kinesis Data Firehose that contains the inspected traffic
-- information, the redacted fields details, and the Amazon Resource Name
-- (ARN) of the web ACL to monitor.
--
-- When specifying @Type@ in @RedactedFields@, you must use one of the
-- following values: @URI@, @QUERY_STRING@, @HEADER@, or @METHOD@.
newPutLoggingConfiguration ::
  -- | 'loggingConfiguration'
  LoggingConfiguration ->
  PutLoggingConfiguration
newPutLoggingConfiguration pLoggingConfiguration_ =
  PutLoggingConfiguration'
    { loggingConfiguration =
        pLoggingConfiguration_
    }

-- | The Amazon Kinesis Data Firehose that contains the inspected traffic
-- information, the redacted fields details, and the Amazon Resource Name
-- (ARN) of the web ACL to monitor.
--
-- When specifying @Type@ in @RedactedFields@, you must use one of the
-- following values: @URI@, @QUERY_STRING@, @HEADER@, or @METHOD@.
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
              Data.=# ( "AWSWAF_20150824.PutLoggingConfiguration" ::
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
  { -- | The LoggingConfiguration that you submitted in the request.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
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
-- 'loggingConfiguration', 'putLoggingConfigurationResponse_loggingConfiguration' - The LoggingConfiguration that you submitted in the request.
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

-- | The LoggingConfiguration that you submitted in the request.
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
    Prelude.rnf loggingConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
