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
-- Module      : Amazonka.SSM.CancelCommand
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel the command specified by the Command ID. There is no
-- guarantee that the command will be terminated and the underlying process
-- stopped.
module Amazonka.SSM.CancelCommand
  ( -- * Creating a Request
    CancelCommand (..),
    newCancelCommand,

    -- * Request Lenses
    cancelCommand_instanceIds,
    cancelCommand_commandId,

    -- * Destructuring the Response
    CancelCommandResponse (..),
    newCancelCommandResponse,

    -- * Response Lenses
    cancelCommandResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- |
--
-- /See:/ 'newCancelCommand' smart constructor.
data CancelCommand = CancelCommand'
  { -- | (Optional) A list of managed node IDs on which you want to cancel the
    -- command. If not provided, the command is canceled on every node on which
    -- it was requested.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the command you want to cancel.
    commandId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCommand' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'cancelCommand_instanceIds' - (Optional) A list of managed node IDs on which you want to cancel the
-- command. If not provided, the command is canceled on every node on which
-- it was requested.
--
-- 'commandId', 'cancelCommand_commandId' - The ID of the command you want to cancel.
newCancelCommand ::
  -- | 'commandId'
  Prelude.Text ->
  CancelCommand
newCancelCommand pCommandId_ =
  CancelCommand'
    { instanceIds = Prelude.Nothing,
      commandId = pCommandId_
    }

-- | (Optional) A list of managed node IDs on which you want to cancel the
-- command. If not provided, the command is canceled on every node on which
-- it was requested.
cancelCommand_instanceIds :: Lens.Lens' CancelCommand (Prelude.Maybe [Prelude.Text])
cancelCommand_instanceIds = Lens.lens (\CancelCommand' {instanceIds} -> instanceIds) (\s@CancelCommand' {} a -> s {instanceIds = a} :: CancelCommand) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the command you want to cancel.
cancelCommand_commandId :: Lens.Lens' CancelCommand Prelude.Text
cancelCommand_commandId = Lens.lens (\CancelCommand' {commandId} -> commandId) (\s@CancelCommand' {} a -> s {commandId = a} :: CancelCommand)

instance Core.AWSRequest CancelCommand where
  type
    AWSResponse CancelCommand =
      CancelCommandResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelCommandResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelCommand where
  hashWithSalt _salt CancelCommand' {..} =
    _salt
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` commandId

instance Prelude.NFData CancelCommand where
  rnf CancelCommand' {..} =
    Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf commandId

instance Data.ToHeaders CancelCommand where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.CancelCommand" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelCommand where
  toJSON CancelCommand' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceIds" Data..=) Prelude.<$> instanceIds,
            Prelude.Just ("CommandId" Data..= commandId)
          ]
      )

instance Data.ToPath CancelCommand where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelCommand where
  toQuery = Prelude.const Prelude.mempty

-- | Whether or not the command was successfully canceled. There is no
-- guarantee that a request can be canceled.
--
-- /See:/ 'newCancelCommandResponse' smart constructor.
data CancelCommandResponse = CancelCommandResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCommandResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelCommandResponse_httpStatus' - The response's http status code.
newCancelCommandResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelCommandResponse
newCancelCommandResponse pHttpStatus_ =
  CancelCommandResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelCommandResponse_httpStatus :: Lens.Lens' CancelCommandResponse Prelude.Int
cancelCommandResponse_httpStatus = Lens.lens (\CancelCommandResponse' {httpStatus} -> httpStatus) (\s@CancelCommandResponse' {} a -> s {httpStatus = a} :: CancelCommandResponse)

instance Prelude.NFData CancelCommandResponse where
  rnf CancelCommandResponse' {..} =
    Prelude.rnf httpStatus
