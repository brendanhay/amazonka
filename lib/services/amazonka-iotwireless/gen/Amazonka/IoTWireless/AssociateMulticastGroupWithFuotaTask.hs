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
-- Module      : Amazonka.IoTWireless.AssociateMulticastGroupWithFuotaTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate a multicast group with a FUOTA task.
module Amazonka.IoTWireless.AssociateMulticastGroupWithFuotaTask
  ( -- * Creating a Request
    AssociateMulticastGroupWithFuotaTask (..),
    newAssociateMulticastGroupWithFuotaTask,

    -- * Request Lenses
    associateMulticastGroupWithFuotaTask_id,
    associateMulticastGroupWithFuotaTask_multicastGroupId,

    -- * Destructuring the Response
    AssociateMulticastGroupWithFuotaTaskResponse (..),
    newAssociateMulticastGroupWithFuotaTaskResponse,

    -- * Response Lenses
    associateMulticastGroupWithFuotaTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateMulticastGroupWithFuotaTask' smart constructor.
data AssociateMulticastGroupWithFuotaTask = AssociateMulticastGroupWithFuotaTask'
  { id :: Prelude.Text,
    multicastGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMulticastGroupWithFuotaTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateMulticastGroupWithFuotaTask_id' - Undocumented member.
--
-- 'multicastGroupId', 'associateMulticastGroupWithFuotaTask_multicastGroupId' - Undocumented member.
newAssociateMulticastGroupWithFuotaTask ::
  -- | 'id'
  Prelude.Text ->
  -- | 'multicastGroupId'
  Prelude.Text ->
  AssociateMulticastGroupWithFuotaTask
newAssociateMulticastGroupWithFuotaTask
  pId_
  pMulticastGroupId_ =
    AssociateMulticastGroupWithFuotaTask'
      { id = pId_,
        multicastGroupId = pMulticastGroupId_
      }

-- | Undocumented member.
associateMulticastGroupWithFuotaTask_id :: Lens.Lens' AssociateMulticastGroupWithFuotaTask Prelude.Text
associateMulticastGroupWithFuotaTask_id = Lens.lens (\AssociateMulticastGroupWithFuotaTask' {id} -> id) (\s@AssociateMulticastGroupWithFuotaTask' {} a -> s {id = a} :: AssociateMulticastGroupWithFuotaTask)

-- | Undocumented member.
associateMulticastGroupWithFuotaTask_multicastGroupId :: Lens.Lens' AssociateMulticastGroupWithFuotaTask Prelude.Text
associateMulticastGroupWithFuotaTask_multicastGroupId = Lens.lens (\AssociateMulticastGroupWithFuotaTask' {multicastGroupId} -> multicastGroupId) (\s@AssociateMulticastGroupWithFuotaTask' {} a -> s {multicastGroupId = a} :: AssociateMulticastGroupWithFuotaTask)

instance
  Core.AWSRequest
    AssociateMulticastGroupWithFuotaTask
  where
  type
    AWSResponse AssociateMulticastGroupWithFuotaTask =
      AssociateMulticastGroupWithFuotaTaskResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateMulticastGroupWithFuotaTaskResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateMulticastGroupWithFuotaTask
  where
  hashWithSalt
    _salt
    AssociateMulticastGroupWithFuotaTask' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` multicastGroupId

instance
  Prelude.NFData
    AssociateMulticastGroupWithFuotaTask
  where
  rnf AssociateMulticastGroupWithFuotaTask' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf multicastGroupId

instance
  Data.ToHeaders
    AssociateMulticastGroupWithFuotaTask
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociateMulticastGroupWithFuotaTask
  where
  toJSON AssociateMulticastGroupWithFuotaTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("MulticastGroupId" Data..= multicastGroupId)
          ]
      )

instance
  Data.ToPath
    AssociateMulticastGroupWithFuotaTask
  where
  toPath AssociateMulticastGroupWithFuotaTask' {..} =
    Prelude.mconcat
      ["/fuota-tasks/", Data.toBS id, "/multicast-group"]

instance
  Data.ToQuery
    AssociateMulticastGroupWithFuotaTask
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateMulticastGroupWithFuotaTaskResponse' smart constructor.
data AssociateMulticastGroupWithFuotaTaskResponse = AssociateMulticastGroupWithFuotaTaskResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateMulticastGroupWithFuotaTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateMulticastGroupWithFuotaTaskResponse_httpStatus' - The response's http status code.
newAssociateMulticastGroupWithFuotaTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateMulticastGroupWithFuotaTaskResponse
newAssociateMulticastGroupWithFuotaTaskResponse
  pHttpStatus_ =
    AssociateMulticastGroupWithFuotaTaskResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateMulticastGroupWithFuotaTaskResponse_httpStatus :: Lens.Lens' AssociateMulticastGroupWithFuotaTaskResponse Prelude.Int
associateMulticastGroupWithFuotaTaskResponse_httpStatus = Lens.lens (\AssociateMulticastGroupWithFuotaTaskResponse' {httpStatus} -> httpStatus) (\s@AssociateMulticastGroupWithFuotaTaskResponse' {} a -> s {httpStatus = a} :: AssociateMulticastGroupWithFuotaTaskResponse)

instance
  Prelude.NFData
    AssociateMulticastGroupWithFuotaTaskResponse
  where
  rnf AssociateMulticastGroupWithFuotaTaskResponse' {..} =
    Prelude.rnf httpStatus
