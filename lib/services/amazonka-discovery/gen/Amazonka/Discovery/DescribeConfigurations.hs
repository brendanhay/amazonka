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
-- Module      : Amazonka.Discovery.DescribeConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes for a list of configuration item IDs.
--
-- All of the supplied IDs must be for the same asset type from one of the
-- following:
--
-- -   server
--
-- -   application
--
-- -   process
--
-- -   connection
--
-- Output fields are specific to the asset type specified. For example, the
-- output for a /server/ configuration item includes a list of attributes
-- about the server, such as host name, operating system, number of network
-- cards, etc.
--
-- For a complete list of outputs for each asset type, see
-- <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html#DescribeConfigurations Using the DescribeConfigurations Action>
-- in the /Amazon Web Services Application Discovery Service User Guide/.
module Amazonka.Discovery.DescribeConfigurations
  ( -- * Creating a Request
    DescribeConfigurations (..),
    newDescribeConfigurations,

    -- * Request Lenses
    describeConfigurations_configurationIds,

    -- * Destructuring the Response
    DescribeConfigurationsResponse (..),
    newDescribeConfigurationsResponse,

    -- * Response Lenses
    describeConfigurationsResponse_configurations,
    describeConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeConfigurations' smart constructor.
data DescribeConfigurations = DescribeConfigurations'
  { -- | One or more configuration IDs.
    configurationIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationIds', 'describeConfigurations_configurationIds' - One or more configuration IDs.
newDescribeConfigurations ::
  DescribeConfigurations
newDescribeConfigurations =
  DescribeConfigurations'
    { configurationIds =
        Prelude.mempty
    }

-- | One or more configuration IDs.
describeConfigurations_configurationIds :: Lens.Lens' DescribeConfigurations [Prelude.Text]
describeConfigurations_configurationIds = Lens.lens (\DescribeConfigurations' {configurationIds} -> configurationIds) (\s@DescribeConfigurations' {} a -> s {configurationIds = a} :: DescribeConfigurations) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeConfigurations where
  type
    AWSResponse DescribeConfigurations =
      DescribeConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationsResponse'
            Prelude.<$> (x Core..?> "configurations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeConfigurations where
  hashWithSalt _salt DescribeConfigurations' {..} =
    _salt `Prelude.hashWithSalt` configurationIds

instance Prelude.NFData DescribeConfigurations where
  rnf DescribeConfigurations' {..} =
    Prelude.rnf configurationIds

instance Core.ToHeaders DescribeConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.DescribeConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeConfigurations where
  toJSON DescribeConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configurationIds" Core..= configurationIds)
          ]
      )

instance Core.ToPath DescribeConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeConfigurationsResponse' smart constructor.
data DescribeConfigurationsResponse = DescribeConfigurationsResponse'
  { -- | A key in the response map. The value is an array of data.
    configurations :: Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'describeConfigurationsResponse_configurations' - A key in the response map. The value is an array of data.
--
-- 'httpStatus', 'describeConfigurationsResponse_httpStatus' - The response's http status code.
newDescribeConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationsResponse
newDescribeConfigurationsResponse pHttpStatus_ =
  DescribeConfigurationsResponse'
    { configurations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A key in the response map. The value is an array of data.
describeConfigurationsResponse_configurations :: Lens.Lens' DescribeConfigurationsResponse (Prelude.Maybe [Prelude.HashMap Prelude.Text Prelude.Text])
describeConfigurationsResponse_configurations = Lens.lens (\DescribeConfigurationsResponse' {configurations} -> configurations) (\s@DescribeConfigurationsResponse' {} a -> s {configurations = a} :: DescribeConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConfigurationsResponse_httpStatus :: Lens.Lens' DescribeConfigurationsResponse Prelude.Int
describeConfigurationsResponse_httpStatus = Lens.lens (\DescribeConfigurationsResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationsResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationsResponse)

instance
  Prelude.NFData
    DescribeConfigurationsResponse
  where
  rnf DescribeConfigurationsResponse' {..} =
    Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf httpStatus
