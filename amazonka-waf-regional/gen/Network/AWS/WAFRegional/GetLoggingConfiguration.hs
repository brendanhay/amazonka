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
-- Module      : Network.AWS.WAFRegional.GetLoggingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- Returns the LoggingConfiguration for the specified web ACL.
module Network.AWS.WAFRegional.GetLoggingConfiguration
  ( -- * Creating a Request
    GetLoggingConfiguration (..),
    newGetLoggingConfiguration,

    -- * Request Lenses
    getLoggingConfiguration_resourceArn,

    -- * Destructuring the Response
    GetLoggingConfigurationResponse (..),
    newGetLoggingConfigurationResponse,

    -- * Response Lenses
    getLoggingConfigurationResponse_loggingConfiguration,
    getLoggingConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newGetLoggingConfiguration' smart constructor.
data GetLoggingConfiguration = GetLoggingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the web ACL for which you want to get
    -- the LoggingConfiguration.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getLoggingConfiguration_resourceArn' - The Amazon Resource Name (ARN) of the web ACL for which you want to get
-- the LoggingConfiguration.
newGetLoggingConfiguration ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetLoggingConfiguration
newGetLoggingConfiguration pResourceArn_ =
  GetLoggingConfiguration'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the web ACL for which you want to get
-- the LoggingConfiguration.
getLoggingConfiguration_resourceArn :: Lens.Lens' GetLoggingConfiguration Prelude.Text
getLoggingConfiguration_resourceArn = Lens.lens (\GetLoggingConfiguration' {resourceArn} -> resourceArn) (\s@GetLoggingConfiguration' {} a -> s {resourceArn = a} :: GetLoggingConfiguration)

instance Core.AWSRequest GetLoggingConfiguration where
  type
    AWSResponse GetLoggingConfiguration =
      GetLoggingConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingConfigurationResponse'
            Prelude.<$> (x Core..?> "LoggingConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoggingConfiguration

instance Prelude.NFData GetLoggingConfiguration

instance Core.ToHeaders GetLoggingConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetLoggingConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLoggingConfiguration where
  toJSON GetLoggingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath GetLoggingConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLoggingConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { -- | The LoggingConfiguration for the specified web ACL.
    loggingConfiguration :: Prelude.Maybe LoggingConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoggingConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingConfiguration', 'getLoggingConfigurationResponse_loggingConfiguration' - The LoggingConfiguration for the specified web ACL.
--
-- 'httpStatus', 'getLoggingConfigurationResponse_httpStatus' - The response's http status code.
newGetLoggingConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoggingConfigurationResponse
newGetLoggingConfigurationResponse pHttpStatus_ =
  GetLoggingConfigurationResponse'
    { loggingConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The LoggingConfiguration for the specified web ACL.
getLoggingConfigurationResponse_loggingConfiguration :: Lens.Lens' GetLoggingConfigurationResponse (Prelude.Maybe LoggingConfiguration)
getLoggingConfigurationResponse_loggingConfiguration = Lens.lens (\GetLoggingConfigurationResponse' {loggingConfiguration} -> loggingConfiguration) (\s@GetLoggingConfigurationResponse' {} a -> s {loggingConfiguration = a} :: GetLoggingConfigurationResponse)

-- | The response's http status code.
getLoggingConfigurationResponse_httpStatus :: Lens.Lens' GetLoggingConfigurationResponse Prelude.Int
getLoggingConfigurationResponse_httpStatus = Lens.lens (\GetLoggingConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetLoggingConfigurationResponse' {} a -> s {httpStatus = a} :: GetLoggingConfigurationResponse)

instance
  Prelude.NFData
    GetLoggingConfigurationResponse
