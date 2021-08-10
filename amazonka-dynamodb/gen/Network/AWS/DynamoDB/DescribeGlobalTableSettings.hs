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
-- Module      : Network.AWS.DynamoDB.DescribeGlobalTableSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Region-specific settings for a global table.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29>
-- of global tables.
module Network.AWS.DynamoDB.DescribeGlobalTableSettings
  ( -- * Creating a Request
    DescribeGlobalTableSettings (..),
    newDescribeGlobalTableSettings,

    -- * Request Lenses
    describeGlobalTableSettings_globalTableName,

    -- * Destructuring the Response
    DescribeGlobalTableSettingsResponse (..),
    newDescribeGlobalTableSettingsResponse,

    -- * Response Lenses
    describeGlobalTableSettingsResponse_replicaSettings,
    describeGlobalTableSettingsResponse_globalTableName,
    describeGlobalTableSettingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalTableSettingsResponse'
            Prelude.<$> ( x Core..?> "ReplicaSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "GlobalTableName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalTableSettings

instance Prelude.NFData DescribeGlobalTableSettings

instance Core.ToHeaders DescribeGlobalTableSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeGlobalTableSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGlobalTableSettings where
  toJSON DescribeGlobalTableSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GlobalTableName" Core..= globalTableName)
          ]
      )

instance Core.ToPath DescribeGlobalTableSettings where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGlobalTableSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGlobalTableSettingsResponse' smart constructor.
data DescribeGlobalTableSettingsResponse = DescribeGlobalTableSettingsResponse'
  { -- | The Region-specific settings for the global table.
    replicaSettings :: Prelude.Maybe [ReplicaSettingsDescription],
    -- | The name of the global table.
    globalTableName :: Prelude.Maybe Prelude.Text,
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
-- 'replicaSettings', 'describeGlobalTableSettingsResponse_replicaSettings' - The Region-specific settings for the global table.
--
-- 'globalTableName', 'describeGlobalTableSettingsResponse_globalTableName' - The name of the global table.
--
-- 'httpStatus', 'describeGlobalTableSettingsResponse_httpStatus' - The response's http status code.
newDescribeGlobalTableSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalTableSettingsResponse
newDescribeGlobalTableSettingsResponse pHttpStatus_ =
  DescribeGlobalTableSettingsResponse'
    { replicaSettings =
        Prelude.Nothing,
      globalTableName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Region-specific settings for the global table.
describeGlobalTableSettingsResponse_replicaSettings :: Lens.Lens' DescribeGlobalTableSettingsResponse (Prelude.Maybe [ReplicaSettingsDescription])
describeGlobalTableSettingsResponse_replicaSettings = Lens.lens (\DescribeGlobalTableSettingsResponse' {replicaSettings} -> replicaSettings) (\s@DescribeGlobalTableSettingsResponse' {} a -> s {replicaSettings = a} :: DescribeGlobalTableSettingsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the global table.
describeGlobalTableSettingsResponse_globalTableName :: Lens.Lens' DescribeGlobalTableSettingsResponse (Prelude.Maybe Prelude.Text)
describeGlobalTableSettingsResponse_globalTableName = Lens.lens (\DescribeGlobalTableSettingsResponse' {globalTableName} -> globalTableName) (\s@DescribeGlobalTableSettingsResponse' {} a -> s {globalTableName = a} :: DescribeGlobalTableSettingsResponse)

-- | The response's http status code.
describeGlobalTableSettingsResponse_httpStatus :: Lens.Lens' DescribeGlobalTableSettingsResponse Prelude.Int
describeGlobalTableSettingsResponse_httpStatus = Lens.lens (\DescribeGlobalTableSettingsResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalTableSettingsResponse' {} a -> s {httpStatus = a} :: DescribeGlobalTableSettingsResponse)

instance
  Prelude.NFData
    DescribeGlobalTableSettingsResponse
