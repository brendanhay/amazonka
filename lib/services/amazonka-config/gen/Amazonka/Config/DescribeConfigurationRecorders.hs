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
-- Module      : Amazonka.Config.DescribeConfigurationRecorders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the specified configuration recorders. If the
-- configuration recorder is not specified, this action returns the details
-- for all configuration recorders associated with the account.
--
-- You can specify only one configuration recorder for each Amazon Web
-- Services Region for each account.
module Amazonka.Config.DescribeConfigurationRecorders
  ( -- * Creating a Request
    DescribeConfigurationRecorders (..),
    newDescribeConfigurationRecorders,

    -- * Request Lenses
    describeConfigurationRecorders_configurationRecorderNames,

    -- * Destructuring the Response
    DescribeConfigurationRecordersResponse (..),
    newDescribeConfigurationRecordersResponse,

    -- * Response Lenses
    describeConfigurationRecordersResponse_configurationRecorders,
    describeConfigurationRecordersResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DescribeConfigurationRecorders action.
--
-- /See:/ 'newDescribeConfigurationRecorders' smart constructor.
data DescribeConfigurationRecorders = DescribeConfigurationRecorders'
  { -- | A list of configuration recorder names.
    configurationRecorderNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationRecorders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorderNames', 'describeConfigurationRecorders_configurationRecorderNames' - A list of configuration recorder names.
newDescribeConfigurationRecorders ::
  DescribeConfigurationRecorders
newDescribeConfigurationRecorders =
  DescribeConfigurationRecorders'
    { configurationRecorderNames =
        Prelude.Nothing
    }

-- | A list of configuration recorder names.
describeConfigurationRecorders_configurationRecorderNames :: Lens.Lens' DescribeConfigurationRecorders (Prelude.Maybe [Prelude.Text])
describeConfigurationRecorders_configurationRecorderNames = Lens.lens (\DescribeConfigurationRecorders' {configurationRecorderNames} -> configurationRecorderNames) (\s@DescribeConfigurationRecorders' {} a -> s {configurationRecorderNames = a} :: DescribeConfigurationRecorders) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    DescribeConfigurationRecorders
  where
  type
    AWSResponse DescribeConfigurationRecorders =
      DescribeConfigurationRecordersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeConfigurationRecordersResponse'
            Prelude.<$> ( x
                            Data..?> "ConfigurationRecorders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeConfigurationRecorders
  where
  hashWithSalt
    _salt
    DescribeConfigurationRecorders' {..} =
      _salt
        `Prelude.hashWithSalt` configurationRecorderNames

instance
  Prelude.NFData
    DescribeConfigurationRecorders
  where
  rnf DescribeConfigurationRecorders' {..} =
    Prelude.rnf configurationRecorderNames

instance
  Data.ToHeaders
    DescribeConfigurationRecorders
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeConfigurationRecorders" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConfigurationRecorders where
  toJSON DescribeConfigurationRecorders' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationRecorderNames" Data..=)
              Prelude.<$> configurationRecorderNames
          ]
      )

instance Data.ToPath DescribeConfigurationRecorders where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConfigurationRecorders where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DescribeConfigurationRecorders action.
--
-- /See:/ 'newDescribeConfigurationRecordersResponse' smart constructor.
data DescribeConfigurationRecordersResponse = DescribeConfigurationRecordersResponse'
  { -- | A list that contains the descriptions of the specified configuration
    -- recorders.
    configurationRecorders :: Prelude.Maybe [ConfigurationRecorder],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConfigurationRecordersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationRecorders', 'describeConfigurationRecordersResponse_configurationRecorders' - A list that contains the descriptions of the specified configuration
-- recorders.
--
-- 'httpStatus', 'describeConfigurationRecordersResponse_httpStatus' - The response's http status code.
newDescribeConfigurationRecordersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeConfigurationRecordersResponse
newDescribeConfigurationRecordersResponse
  pHttpStatus_ =
    DescribeConfigurationRecordersResponse'
      { configurationRecorders =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list that contains the descriptions of the specified configuration
-- recorders.
describeConfigurationRecordersResponse_configurationRecorders :: Lens.Lens' DescribeConfigurationRecordersResponse (Prelude.Maybe [ConfigurationRecorder])
describeConfigurationRecordersResponse_configurationRecorders = Lens.lens (\DescribeConfigurationRecordersResponse' {configurationRecorders} -> configurationRecorders) (\s@DescribeConfigurationRecordersResponse' {} a -> s {configurationRecorders = a} :: DescribeConfigurationRecordersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeConfigurationRecordersResponse_httpStatus :: Lens.Lens' DescribeConfigurationRecordersResponse Prelude.Int
describeConfigurationRecordersResponse_httpStatus = Lens.lens (\DescribeConfigurationRecordersResponse' {httpStatus} -> httpStatus) (\s@DescribeConfigurationRecordersResponse' {} a -> s {httpStatus = a} :: DescribeConfigurationRecordersResponse)

instance
  Prelude.NFData
    DescribeConfigurationRecordersResponse
  where
  rnf DescribeConfigurationRecordersResponse' {..} =
    Prelude.rnf configurationRecorders
      `Prelude.seq` Prelude.rnf httpStatus
