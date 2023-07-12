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
-- Module      : Amazonka.DynamoDB.DescribeGlobalTableSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Region-specific settings for a global table.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29>
-- of global tables.
module Amazonka.DynamoDB.DescribeGlobalTableSettings
  ( -- * Creating a Request
    DescribeGlobalTableSettings (..),
    newDescribeGlobalTableSettings,

    -- * Request Lenses
    describeGlobalTableSettings_globalTableName,

    -- * Destructuring the Response
    DescribeGlobalTableSettingsResponse (..),
    newDescribeGlobalTableSettingsResponse,

    -- * Response Lenses
    describeGlobalTableSettingsResponse_globalTableName,
    describeGlobalTableSettingsResponse_replicaSettings,
    describeGlobalTableSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalTableSettings' smart constructor.
data DescribeGlobalTableSettings = DescribeGlobalTableSettings'
  { -- | The name of the global table to describe.
    globalTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalTableSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableName', 'describeGlobalTableSettings_globalTableName' - The name of the global table to describe.
newDescribeGlobalTableSettings ::
  -- | 'globalTableName'
  Prelude.Text ->
  DescribeGlobalTableSettings
newDescribeGlobalTableSettings pGlobalTableName_ =
  DescribeGlobalTableSettings'
    { globalTableName =
        pGlobalTableName_
    }

-- | The name of the global table to describe.
describeGlobalTableSettings_globalTableName :: Lens.Lens' DescribeGlobalTableSettings Prelude.Text
describeGlobalTableSettings_globalTableName = Lens.lens (\DescribeGlobalTableSettings' {globalTableName} -> globalTableName) (\s@DescribeGlobalTableSettings' {} a -> s {globalTableName = a} :: DescribeGlobalTableSettings)

instance Core.AWSRequest DescribeGlobalTableSettings where
  type
    AWSResponse DescribeGlobalTableSettings =
      DescribeGlobalTableSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalTableSettingsResponse'
            Prelude.<$> (x Data..?> "GlobalTableName")
            Prelude.<*> ( x
                            Data..?> "ReplicaSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalTableSettings where
  hashWithSalt _salt DescribeGlobalTableSettings' {..} =
    _salt `Prelude.hashWithSalt` globalTableName

instance Prelude.NFData DescribeGlobalTableSettings where
  rnf DescribeGlobalTableSettings' {..} =
    Prelude.rnf globalTableName

instance Data.ToHeaders DescribeGlobalTableSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeGlobalTableSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGlobalTableSettings where
  toJSON DescribeGlobalTableSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GlobalTableName" Data..= globalTableName)
          ]
      )

instance Data.ToPath DescribeGlobalTableSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGlobalTableSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGlobalTableSettingsResponse' smart constructor.
data DescribeGlobalTableSettingsResponse = DescribeGlobalTableSettingsResponse'
  { -- | The name of the global table.
    globalTableName :: Prelude.Maybe Prelude.Text,
    -- | The Region-specific settings for the global table.
    replicaSettings :: Prelude.Maybe [ReplicaSettingsDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalTableSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableName', 'describeGlobalTableSettingsResponse_globalTableName' - The name of the global table.
--
-- 'replicaSettings', 'describeGlobalTableSettingsResponse_replicaSettings' - The Region-specific settings for the global table.
--
-- 'httpStatus', 'describeGlobalTableSettingsResponse_httpStatus' - The response's http status code.
newDescribeGlobalTableSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalTableSettingsResponse
newDescribeGlobalTableSettingsResponse pHttpStatus_ =
  DescribeGlobalTableSettingsResponse'
    { globalTableName =
        Prelude.Nothing,
      replicaSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the global table.
describeGlobalTableSettingsResponse_globalTableName :: Lens.Lens' DescribeGlobalTableSettingsResponse (Prelude.Maybe Prelude.Text)
describeGlobalTableSettingsResponse_globalTableName = Lens.lens (\DescribeGlobalTableSettingsResponse' {globalTableName} -> globalTableName) (\s@DescribeGlobalTableSettingsResponse' {} a -> s {globalTableName = a} :: DescribeGlobalTableSettingsResponse)

-- | The Region-specific settings for the global table.
describeGlobalTableSettingsResponse_replicaSettings :: Lens.Lens' DescribeGlobalTableSettingsResponse (Prelude.Maybe [ReplicaSettingsDescription])
describeGlobalTableSettingsResponse_replicaSettings = Lens.lens (\DescribeGlobalTableSettingsResponse' {replicaSettings} -> replicaSettings) (\s@DescribeGlobalTableSettingsResponse' {} a -> s {replicaSettings = a} :: DescribeGlobalTableSettingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeGlobalTableSettingsResponse_httpStatus :: Lens.Lens' DescribeGlobalTableSettingsResponse Prelude.Int
describeGlobalTableSettingsResponse_httpStatus = Lens.lens (\DescribeGlobalTableSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalTableSettingsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalTableSettingsResponse)

instance
  Prelude.NFData
    DescribeGlobalTableSettingsResponse
  where
  rnf DescribeGlobalTableSettingsResponse' {..} =
    Prelude.rnf globalTableName
      `Prelude.seq` Prelude.rnf replicaSettings
      `Prelude.seq` Prelude.rnf httpStatus
