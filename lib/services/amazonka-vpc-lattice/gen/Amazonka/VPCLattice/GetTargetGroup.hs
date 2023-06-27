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
-- Module      : Amazonka.VPCLattice.GetTargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified target group.
module Amazonka.VPCLattice.GetTargetGroup
  ( -- * Creating a Request
    GetTargetGroup (..),
    newGetTargetGroup,

    -- * Request Lenses
    getTargetGroup_targetGroupIdentifier,

    -- * Destructuring the Response
    GetTargetGroupResponse (..),
    newGetTargetGroupResponse,

    -- * Response Lenses
    getTargetGroupResponse_arn,
    getTargetGroupResponse_config,
    getTargetGroupResponse_createdAt,
    getTargetGroupResponse_failureCode,
    getTargetGroupResponse_failureMessage,
    getTargetGroupResponse_id,
    getTargetGroupResponse_lastUpdatedAt,
    getTargetGroupResponse_name,
    getTargetGroupResponse_serviceArns,
    getTargetGroupResponse_status,
    getTargetGroupResponse_type,
    getTargetGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newGetTargetGroup' smart constructor.
data GetTargetGroup = GetTargetGroup'
  { -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupIdentifier', 'getTargetGroup_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
newGetTargetGroup ::
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  GetTargetGroup
newGetTargetGroup pTargetGroupIdentifier_ =
  GetTargetGroup'
    { targetGroupIdentifier =
        pTargetGroupIdentifier_
    }

-- | The ID or Amazon Resource Name (ARN) of the target group.
getTargetGroup_targetGroupIdentifier :: Lens.Lens' GetTargetGroup Prelude.Text
getTargetGroup_targetGroupIdentifier = Lens.lens (\GetTargetGroup' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@GetTargetGroup' {} a -> s {targetGroupIdentifier = a} :: GetTargetGroup)

instance Core.AWSRequest GetTargetGroup where
  type
    AWSResponse GetTargetGroup =
      GetTargetGroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTargetGroupResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "config")
            Prelude.<*> (x Data..?> "createdAt")
            Prelude.<*> (x Data..?> "failureCode")
            Prelude.<*> (x Data..?> "failureMessage")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "serviceArns" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTargetGroup where
  hashWithSalt _salt GetTargetGroup' {..} =
    _salt `Prelude.hashWithSalt` targetGroupIdentifier

instance Prelude.NFData GetTargetGroup where
  rnf GetTargetGroup' {..} =
    Prelude.rnf targetGroupIdentifier

instance Data.ToHeaders GetTargetGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTargetGroup where
  toPath GetTargetGroup' {..} =
    Prelude.mconcat
      ["/targetgroups/", Data.toBS targetGroupIdentifier]

instance Data.ToQuery GetTargetGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTargetGroupResponse' smart constructor.
data GetTargetGroupResponse = GetTargetGroupResponse'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The target group configuration.
    config :: Prelude.Maybe TargetGroupConfig,
    -- | The date and time that the target group was created, specified in
    -- ISO-8601 format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The failure code.
    failureCode :: Prelude.Maybe Prelude.Text,
    -- | The failure message.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the target group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the target group was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the target group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of the service.
    serviceArns :: Prelude.Maybe [Prelude.Text],
    -- | The status.
    status :: Prelude.Maybe TargetGroupStatus,
    -- | The target group type.
    type' :: Prelude.Maybe TargetGroupType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTargetGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getTargetGroupResponse_arn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'config', 'getTargetGroupResponse_config' - The target group configuration.
--
-- 'createdAt', 'getTargetGroupResponse_createdAt' - The date and time that the target group was created, specified in
-- ISO-8601 format.
--
-- 'failureCode', 'getTargetGroupResponse_failureCode' - The failure code.
--
-- 'failureMessage', 'getTargetGroupResponse_failureMessage' - The failure message.
--
-- 'id', 'getTargetGroupResponse_id' - The ID of the target group.
--
-- 'lastUpdatedAt', 'getTargetGroupResponse_lastUpdatedAt' - The date and time that the target group was last updated, specified in
-- ISO-8601 format.
--
-- 'name', 'getTargetGroupResponse_name' - The name of the target group.
--
-- 'serviceArns', 'getTargetGroupResponse_serviceArns' - The Amazon Resource Names (ARNs) of the service.
--
-- 'status', 'getTargetGroupResponse_status' - The status.
--
-- 'type'', 'getTargetGroupResponse_type' - The target group type.
--
-- 'httpStatus', 'getTargetGroupResponse_httpStatus' - The response's http status code.
newGetTargetGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTargetGroupResponse
newGetTargetGroupResponse pHttpStatus_ =
  GetTargetGroupResponse'
    { arn = Prelude.Nothing,
      config = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureMessage = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      serviceArns = Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the target group.
getTargetGroupResponse_arn :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.Text)
getTargetGroupResponse_arn = Lens.lens (\GetTargetGroupResponse' {arn} -> arn) (\s@GetTargetGroupResponse' {} a -> s {arn = a} :: GetTargetGroupResponse)

-- | The target group configuration.
getTargetGroupResponse_config :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe TargetGroupConfig)
getTargetGroupResponse_config = Lens.lens (\GetTargetGroupResponse' {config} -> config) (\s@GetTargetGroupResponse' {} a -> s {config = a} :: GetTargetGroupResponse)

-- | The date and time that the target group was created, specified in
-- ISO-8601 format.
getTargetGroupResponse_createdAt :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.UTCTime)
getTargetGroupResponse_createdAt = Lens.lens (\GetTargetGroupResponse' {createdAt} -> createdAt) (\s@GetTargetGroupResponse' {} a -> s {createdAt = a} :: GetTargetGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The failure code.
getTargetGroupResponse_failureCode :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.Text)
getTargetGroupResponse_failureCode = Lens.lens (\GetTargetGroupResponse' {failureCode} -> failureCode) (\s@GetTargetGroupResponse' {} a -> s {failureCode = a} :: GetTargetGroupResponse)

-- | The failure message.
getTargetGroupResponse_failureMessage :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.Text)
getTargetGroupResponse_failureMessage = Lens.lens (\GetTargetGroupResponse' {failureMessage} -> failureMessage) (\s@GetTargetGroupResponse' {} a -> s {failureMessage = a} :: GetTargetGroupResponse)

-- | The ID of the target group.
getTargetGroupResponse_id :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.Text)
getTargetGroupResponse_id = Lens.lens (\GetTargetGroupResponse' {id} -> id) (\s@GetTargetGroupResponse' {} a -> s {id = a} :: GetTargetGroupResponse)

-- | The date and time that the target group was last updated, specified in
-- ISO-8601 format.
getTargetGroupResponse_lastUpdatedAt :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.UTCTime)
getTargetGroupResponse_lastUpdatedAt = Lens.lens (\GetTargetGroupResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetTargetGroupResponse' {} a -> s {lastUpdatedAt = a} :: GetTargetGroupResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the target group.
getTargetGroupResponse_name :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe Prelude.Text)
getTargetGroupResponse_name = Lens.lens (\GetTargetGroupResponse' {name} -> name) (\s@GetTargetGroupResponse' {} a -> s {name = a} :: GetTargetGroupResponse)

-- | The Amazon Resource Names (ARNs) of the service.
getTargetGroupResponse_serviceArns :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe [Prelude.Text])
getTargetGroupResponse_serviceArns = Lens.lens (\GetTargetGroupResponse' {serviceArns} -> serviceArns) (\s@GetTargetGroupResponse' {} a -> s {serviceArns = a} :: GetTargetGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status.
getTargetGroupResponse_status :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe TargetGroupStatus)
getTargetGroupResponse_status = Lens.lens (\GetTargetGroupResponse' {status} -> status) (\s@GetTargetGroupResponse' {} a -> s {status = a} :: GetTargetGroupResponse)

-- | The target group type.
getTargetGroupResponse_type :: Lens.Lens' GetTargetGroupResponse (Prelude.Maybe TargetGroupType)
getTargetGroupResponse_type = Lens.lens (\GetTargetGroupResponse' {type'} -> type') (\s@GetTargetGroupResponse' {} a -> s {type' = a} :: GetTargetGroupResponse)

-- | The response's http status code.
getTargetGroupResponse_httpStatus :: Lens.Lens' GetTargetGroupResponse Prelude.Int
getTargetGroupResponse_httpStatus = Lens.lens (\GetTargetGroupResponse' {httpStatus} -> httpStatus) (\s@GetTargetGroupResponse' {} a -> s {httpStatus = a} :: GetTargetGroupResponse)

instance Prelude.NFData GetTargetGroupResponse where
  rnf GetTargetGroupResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf config
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf failureCode
      `Prelude.seq` Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serviceArns
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
