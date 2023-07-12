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
-- Module      : Amazonka.AppRunner.DescribeObservabilityConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a full description of an App Runner observability configuration
-- resource.
module Amazonka.AppRunner.DescribeObservabilityConfiguration
  ( -- * Creating a Request
    DescribeObservabilityConfiguration (..),
    newDescribeObservabilityConfiguration,

    -- * Request Lenses
    describeObservabilityConfiguration_observabilityConfigurationArn,

    -- * Destructuring the Response
    DescribeObservabilityConfigurationResponse (..),
    newDescribeObservabilityConfigurationResponse,

    -- * Response Lenses
    describeObservabilityConfigurationResponse_httpStatus,
    describeObservabilityConfigurationResponse_observabilityConfiguration,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeObservabilityConfiguration' smart constructor.
data DescribeObservabilityConfiguration = DescribeObservabilityConfiguration'
  { -- | The Amazon Resource Name (ARN) of the App Runner observability
    -- configuration that you want a description for.
    --
    -- The ARN can be a full observability configuration ARN, or a partial ARN
    -- ending with either @...\/@/@name@/@ @ or
    -- @...\/@/@name@/@\/@/@revision@/@ @. If a revision isn\'t specified, the
    -- latest active revision is described.
    observabilityConfigurationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeObservabilityConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observabilityConfigurationArn', 'describeObservabilityConfiguration_observabilityConfigurationArn' - The Amazon Resource Name (ARN) of the App Runner observability
-- configuration that you want a description for.
--
-- The ARN can be a full observability configuration ARN, or a partial ARN
-- ending with either @...\/@/@name@/@ @ or
-- @...\/@/@name@/@\/@/@revision@/@ @. If a revision isn\'t specified, the
-- latest active revision is described.
newDescribeObservabilityConfiguration ::
  -- | 'observabilityConfigurationArn'
  Prelude.Text ->
  DescribeObservabilityConfiguration
newDescribeObservabilityConfiguration
  pObservabilityConfigurationArn_ =
    DescribeObservabilityConfiguration'
      { observabilityConfigurationArn =
          pObservabilityConfigurationArn_
      }

-- | The Amazon Resource Name (ARN) of the App Runner observability
-- configuration that you want a description for.
--
-- The ARN can be a full observability configuration ARN, or a partial ARN
-- ending with either @...\/@/@name@/@ @ or
-- @...\/@/@name@/@\/@/@revision@/@ @. If a revision isn\'t specified, the
-- latest active revision is described.
describeObservabilityConfiguration_observabilityConfigurationArn :: Lens.Lens' DescribeObservabilityConfiguration Prelude.Text
describeObservabilityConfiguration_observabilityConfigurationArn = Lens.lens (\DescribeObservabilityConfiguration' {observabilityConfigurationArn} -> observabilityConfigurationArn) (\s@DescribeObservabilityConfiguration' {} a -> s {observabilityConfigurationArn = a} :: DescribeObservabilityConfiguration)

instance
  Core.AWSRequest
    DescribeObservabilityConfiguration
  where
  type
    AWSResponse DescribeObservabilityConfiguration =
      DescribeObservabilityConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeObservabilityConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ObservabilityConfiguration")
      )

instance
  Prelude.Hashable
    DescribeObservabilityConfiguration
  where
  hashWithSalt
    _salt
    DescribeObservabilityConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` observabilityConfigurationArn

instance
  Prelude.NFData
    DescribeObservabilityConfiguration
  where
  rnf DescribeObservabilityConfiguration' {..} =
    Prelude.rnf observabilityConfigurationArn

instance
  Data.ToHeaders
    DescribeObservabilityConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AppRunner.DescribeObservabilityConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeObservabilityConfiguration
  where
  toJSON DescribeObservabilityConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ObservabilityConfigurationArn"
                  Data..= observabilityConfigurationArn
              )
          ]
      )

instance
  Data.ToPath
    DescribeObservabilityConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeObservabilityConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeObservabilityConfigurationResponse' smart constructor.
data DescribeObservabilityConfigurationResponse = DescribeObservabilityConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the App Runner observability configuration that
    -- you specified in this request.
    observabilityConfiguration :: ObservabilityConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeObservabilityConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeObservabilityConfigurationResponse_httpStatus' - The response's http status code.
--
-- 'observabilityConfiguration', 'describeObservabilityConfigurationResponse_observabilityConfiguration' - A full description of the App Runner observability configuration that
-- you specified in this request.
newDescribeObservabilityConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'observabilityConfiguration'
  ObservabilityConfiguration ->
  DescribeObservabilityConfigurationResponse
newDescribeObservabilityConfigurationResponse
  pHttpStatus_
  pObservabilityConfiguration_ =
    DescribeObservabilityConfigurationResponse'
      { httpStatus =
          pHttpStatus_,
        observabilityConfiguration =
          pObservabilityConfiguration_
      }

-- | The response's http status code.
describeObservabilityConfigurationResponse_httpStatus :: Lens.Lens' DescribeObservabilityConfigurationResponse Prelude.Int
describeObservabilityConfigurationResponse_httpStatus = Lens.lens (\DescribeObservabilityConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeObservabilityConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeObservabilityConfigurationResponse)

-- | A full description of the App Runner observability configuration that
-- you specified in this request.
describeObservabilityConfigurationResponse_observabilityConfiguration :: Lens.Lens' DescribeObservabilityConfigurationResponse ObservabilityConfiguration
describeObservabilityConfigurationResponse_observabilityConfiguration = Lens.lens (\DescribeObservabilityConfigurationResponse' {observabilityConfiguration} -> observabilityConfiguration) (\s@DescribeObservabilityConfigurationResponse' {} a -> s {observabilityConfiguration = a} :: DescribeObservabilityConfigurationResponse)

instance
  Prelude.NFData
    DescribeObservabilityConfigurationResponse
  where
  rnf DescribeObservabilityConfigurationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf observabilityConfiguration
