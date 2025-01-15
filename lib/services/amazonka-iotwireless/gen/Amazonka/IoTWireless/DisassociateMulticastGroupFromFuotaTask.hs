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
-- Module      : Amazonka.IoTWireless.DisassociateMulticastGroupFromFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a multicast group from a fuota task.
module Amazonka.IoTWireless.DisassociateMulticastGroupFromFuotaTask
  ( -- * Creating a Request
    DisassociateMulticastGroupFromFuotaTask (..),
    newDisassociateMulticastGroupFromFuotaTask,

    -- * Request Lenses
    disassociateMulticastGroupFromFuotaTask_id,
    disassociateMulticastGroupFromFuotaTask_multicastGroupId,

    -- * Destructuring the Response
    DisassociateMulticastGroupFromFuotaTaskResponse (..),
    newDisassociateMulticastGroupFromFuotaTaskResponse,

    -- * Response Lenses
    disassociateMulticastGroupFromFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMulticastGroupFromFuotaTask' smart constructor.
data DisassociateMulticastGroupFromFuotaTask = DisassociateMulticastGroupFromFuotaTask'
  { id :: Prelude.Text,
    multicastGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMulticastGroupFromFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateMulticastGroupFromFuotaTask_id' - Undocumented member.
--
-- 'multicastGroupId', 'disassociateMulticastGroupFromFuotaTask_multicastGroupId' - Undocumented member.
newDisassociateMulticastGroupFromFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'multicastGroupId'
  Prelude.Text ->
  DisassociateMulticastGroupFromFuotaTask
newDisassociateMulticastGroupFromFuotaTask
  pId_
  pMulticastGroupId_ =
    DisassociateMulticastGroupFromFuotaTask'
      { id = pId_,
        multicastGroupId =
          pMulticastGroupId_
      }

-- | Undocumented member.
disassociateMulticastGroupFromFuotaTask_id :: Lens.Lens' DisassociateMulticastGroupFromFuotaTask Prelude.Text
disassociateMulticastGroupFromFuotaTask_id = Lens.lens (\DisassociateMulticastGroupFromFuotaTask' {id} -> id) (\s@DisassociateMulticastGroupFromFuotaTask' {} a -> s {id = a} :: DisassociateMulticastGroupFromFuotaTask)

-- | Undocumented member.
disassociateMulticastGroupFromFuotaTask_multicastGroupId :: Lens.Lens' DisassociateMulticastGroupFromFuotaTask Prelude.Text
disassociateMulticastGroupFromFuotaTask_multicastGroupId = Lens.lens (\DisassociateMulticastGroupFromFuotaTask' {multicastGroupId} -> multicastGroupId) (\s@DisassociateMulticastGroupFromFuotaTask' {} a -> s {multicastGroupId = a} :: DisassociateMulticastGroupFromFuotaTask)

instance
  Core.AWSRequest
    DisassociateMulticastGroupFromFuotaTask
  where
  type
    AWSResponse
      DisassociateMulticastGroupFromFuotaTask =
      DisassociateMulticastGroupFromFuotaTaskResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateMulticastGroupFromFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateMulticastGroupFromFuotaTask
  where
  hashWithSalt
    _salt
    DisassociateMulticastGroupFromFuotaTask' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` multicastGroupId

instance
  Prelude.NFData
    DisassociateMulticastGroupFromFuotaTask
  where
  rnf DisassociateMulticastGroupFromFuotaTask' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf multicastGroupId

instance
  Data.ToHeaders
    DisassociateMulticastGroupFromFuotaTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DisassociateMulticastGroupFromFuotaTask
  where
  toPath DisassociateMulticastGroupFromFuotaTask' {..} =
    Prelude.mconcat
      [ "/fuota-tasks/",
        Data.toBS id,
        "/multicast-groups/",
        Data.toBS multicastGroupId
      ]

instance
  Data.ToQuery
    DisassociateMulticastGroupFromFuotaTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMulticastGroupFromFuotaTaskResponse' smart constructor.
data DisassociateMulticastGroupFromFuotaTaskResponse = DisassociateMulticastGroupFromFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMulticastGroupFromFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateMulticastGroupFromFuotaTaskResponse_httpStatus' - The response's http status code.
newDisassociateMulticastGroupFromFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateMulticastGroupFromFuotaTaskResponse
newDisassociateMulticastGroupFromFuotaTaskResponse
  pHttpStatus_ =
    DisassociateMulticastGroupFromFuotaTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateMulticastGroupFromFuotaTaskResponse_httpStatus :: Lens.Lens' DisassociateMulticastGroupFromFuotaTaskResponse Prelude.Int
disassociateMulticastGroupFromFuotaTaskResponse_httpStatus = Lens.lens (\DisassociateMulticastGroupFromFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@DisassociateMulticastGroupFromFuotaTaskResponse' {} a -> s {httpStatus = a} :: DisassociateMulticastGroupFromFuotaTaskResponse)

instance
  Prelude.NFData
    DisassociateMulticastGroupFromFuotaTaskResponse
  where
  rnf
    DisassociateMulticastGroupFromFuotaTaskResponse' {..} =
      Prelude.rnf httpStatus
