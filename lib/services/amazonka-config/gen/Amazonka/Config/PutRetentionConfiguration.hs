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
-- Module      : Amazonka.Config.PutRetentionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates and updates the retention configuration with details about
-- retention period (number of days) that Config stores your historical
-- information. The API creates the @RetentionConfiguration@ object and
-- names the object as __default__. When you have a
-- @RetentionConfiguration@ object named __default__, calling the API
-- modifies the default object.
--
-- Currently, Config supports only one retention configuration per region
-- in your account.
module Amazonka.Config.PutRetentionConfiguration
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRetentionConfiguration' smart constructor.
data PutRetentionConfiguration = PutRetentionConfiguration'
  { -- | Number of days Config stores your historical information.
    --
    -- Currently, only applicable to the configuration item history.
    retentionPeriodInDays :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRetentionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriodInDays', 'putRetentionConfiguration_retentionPeriodInDays' - Number of days Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
newPutRetentionConfiguration ::
  -- | 'retentionPeriodInDays'
  Prelude.Natural ->
  PutRetentionConfiguration
newPutRetentionConfiguration pRetentionPeriodInDays_ =
  PutRetentionConfiguration'
    { retentionPeriodInDays =
        pRetentionPeriodInDays_
    }

-- | Number of days Config stores your historical information.
--
-- Currently, only applicable to the configuration item history.
putRetentionConfiguration_retentionPeriodInDays :: Lens.Lens' PutRetentionConfiguration Prelude.Natural
putRetentionConfiguration_retentionPeriodInDays = Lens.lens (\PutRetentionConfiguration' {retentionPeriodInDays} -> retentionPeriodInDays) (\s@PutRetentionConfiguration' {} a -> s {retentionPeriodInDays = a} :: PutRetentionConfiguration)

instance Core.AWSRequest PutRetentionConfiguration where
  type
    AWSResponse PutRetentionConfiguration =
      PutRetentionConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRetentionConfigurationResponse'
            Prelude.<$> (x Data..?> "RetentionConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRetentionConfiguration where
  hashWithSalt _salt PutRetentionConfiguration' {..} =
    _salt `Prelude.hashWithSalt` retentionPeriodInDays

instance Prelude.NFData PutRetentionConfiguration where
  rnf PutRetentionConfiguration' {..} =
    Prelude.rnf retentionPeriodInDays

instance Data.ToHeaders PutRetentionConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutRetentionConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutRetentionConfiguration where
  toJSON PutRetentionConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RetentionPeriodInDays"
                  Data..= retentionPeriodInDays
              )
          ]
      )

instance Data.ToPath PutRetentionConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutRetentionConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRetentionConfigurationResponse' smart constructor.
data PutRetentionConfigurationResponse = PutRetentionConfigurationResponse'
  { -- | Returns a retention configuration object.
    retentionConfiguration :: Prelude.Maybe RetentionConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutRetentionConfigurationResponse
newPutRetentionConfigurationResponse pHttpStatus_ =
  PutRetentionConfigurationResponse'
    { retentionConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a retention configuration object.
putRetentionConfigurationResponse_retentionConfiguration :: Lens.Lens' PutRetentionConfigurationResponse (Prelude.Maybe RetentionConfiguration)
putRetentionConfigurationResponse_retentionConfiguration = Lens.lens (\PutRetentionConfigurationResponse' {retentionConfiguration} -> retentionConfiguration) (\s@PutRetentionConfigurationResponse' {} a -> s {retentionConfiguration = a} :: PutRetentionConfigurationResponse)

-- | The response's http status code.
putRetentionConfigurationResponse_httpStatus :: Lens.Lens' PutRetentionConfigurationResponse Prelude.Int
putRetentionConfigurationResponse_httpStatus = Lens.lens (\PutRetentionConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutRetentionConfigurationResponse' {} a -> s {httpStatus = a} :: PutRetentionConfigurationResponse)

instance
  Prelude.NFData
    PutRetentionConfigurationResponse
  where
  rnf PutRetentionConfigurationResponse' {..} =
    Prelude.rnf retentionConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
