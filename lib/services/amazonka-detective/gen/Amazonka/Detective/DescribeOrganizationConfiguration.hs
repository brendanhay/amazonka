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
-- Module      : Amazonka.Detective.DescribeOrganizationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the configuration for the organization
-- behavior graph. Currently indicates whether to automatically enable new
-- organization accounts as member accounts.
--
-- Can only be called by the Detective administrator account for the
-- organization.
module Amazonka.Detective.DescribeOrganizationConfiguration
  ( -- * Creating a Request
    DescribeOrganizationConfiguration (..),
    newDescribeOrganizationConfiguration,

    -- * Request Lenses
    describeOrganizationConfiguration_graphArn,

    -- * Destructuring the Response
    DescribeOrganizationConfigurationResponse (..),
    newDescribeOrganizationConfigurationResponse,

    -- * Response Lenses
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationConfiguration' smart constructor.
data DescribeOrganizationConfiguration = DescribeOrganizationConfiguration'
  { -- | The ARN of the organization behavior graph.
    graphArn :: Prelude.Text
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
-- 'graphArn', 'describeOrganizationConfiguration_graphArn' - The ARN of the organization behavior graph.
newDescribeOrganizationConfiguration ::
  -- | 'graphArn'
  Prelude.Text ->
  DescribeOrganizationConfiguration
newDescribeOrganizationConfiguration pGraphArn_ =
  DescribeOrganizationConfiguration'
    { graphArn =
        pGraphArn_
    }

-- | The ARN of the organization behavior graph.
describeOrganizationConfiguration_graphArn :: Lens.Lens' DescribeOrganizationConfiguration Prelude.Text
describeOrganizationConfiguration_graphArn = Lens.lens (\DescribeOrganizationConfiguration' {graphArn} -> graphArn) (\s@DescribeOrganizationConfiguration' {} a -> s {graphArn = a} :: DescribeOrganizationConfiguration)

instance
  Core.AWSRequest
    DescribeOrganizationConfiguration
  where
  type
    AWSResponse DescribeOrganizationConfiguration =
      DescribeOrganizationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigurationResponse'
            Prelude.<$> (x Data..?> "AutoEnable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfiguration
  where
  hashWithSalt
    _salt
    DescribeOrganizationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` graphArn

instance
  Prelude.NFData
    DescribeOrganizationConfiguration
  where
  rnf DescribeOrganizationConfiguration' {..} =
    Prelude.rnf graphArn

instance
  Data.ToHeaders
    DescribeOrganizationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeOrganizationConfiguration
  where
  toJSON DescribeOrganizationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GraphArn" Data..= graphArn)]
      )

instance
  Data.ToPath
    DescribeOrganizationConfiguration
  where
  toPath =
    Prelude.const
      "/orgs/describeOrganizationConfiguration"

instance
  Data.ToQuery
    DescribeOrganizationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConfigurationResponse' smart constructor.
data DescribeOrganizationConfigurationResponse = DescribeOrganizationConfigurationResponse'
  { -- | Indicates whether to automatically enable new organization accounts as
    -- member accounts in the organization behavior graph.
    autoEnable :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'autoEnable', 'describeOrganizationConfigurationResponse_autoEnable' - Indicates whether to automatically enable new organization accounts as
-- member accounts in the organization behavior graph.
--
-- 'httpStatus', 'describeOrganizationConfigurationResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConfigurationResponse
newDescribeOrganizationConfigurationResponse
  pHttpStatus_ =
    DescribeOrganizationConfigurationResponse'
      { autoEnable =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Indicates whether to automatically enable new organization accounts as
-- member accounts in the organization behavior graph.
describeOrganizationConfigurationResponse_autoEnable :: Lens.Lens' DescribeOrganizationConfigurationResponse (Prelude.Maybe Prelude.Bool)
describeOrganizationConfigurationResponse_autoEnable = Lens.lens (\DescribeOrganizationConfigurationResponse' {autoEnable} -> autoEnable) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {autoEnable = a} :: DescribeOrganizationConfigurationResponse)

-- | The response's http status code.
describeOrganizationConfigurationResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigurationResponse Prelude.Int
describeOrganizationConfigurationResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigurationResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigurationResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigurationResponse
  where
  rnf DescribeOrganizationConfigurationResponse' {..} =
    Prelude.rnf autoEnable `Prelude.seq`
      Prelude.rnf httpStatus
