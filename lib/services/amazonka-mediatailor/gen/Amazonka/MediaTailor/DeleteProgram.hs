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
-- Module      : Amazonka.MediaTailor.DeleteProgram
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific program on a specific channel.
module Amazonka.MediaTailor.DeleteProgram
  ( -- * Creating a Request
    DeleteProgram (..),
    newDeleteProgram,

    -- * Request Lenses
    deleteProgram_channelName,
    deleteProgram_programName,

    -- * Destructuring the Response
    DeleteProgramResponse (..),
    newDeleteProgramResponse,

    -- * Response Lenses
    deleteProgramResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaTailor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProgram' smart constructor.
data DeleteProgram = DeleteProgram'
  { -- | The identifier for the channel you are working on.
    channelName :: Prelude.Text,
    -- | The identifier for the program you are working on.
    programName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProgram' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'deleteProgram_channelName' - The identifier for the channel you are working on.
--
-- 'programName', 'deleteProgram_programName' - The identifier for the program you are working on.
newDeleteProgram ::
  -- | 'channelName'
  Prelude.Text ->
  -- | 'programName'
  Prelude.Text ->
  DeleteProgram
newDeleteProgram pChannelName_ pProgramName_ =
  DeleteProgram'
    { channelName = pChannelName_,
      programName = pProgramName_
    }

-- | The identifier for the channel you are working on.
deleteProgram_channelName :: Lens.Lens' DeleteProgram Prelude.Text
deleteProgram_channelName = Lens.lens (\DeleteProgram' {channelName} -> channelName) (\s@DeleteProgram' {} a -> s {channelName = a} :: DeleteProgram)

-- | The identifier for the program you are working on.
deleteProgram_programName :: Lens.Lens' DeleteProgram Prelude.Text
deleteProgram_programName = Lens.lens (\DeleteProgram' {programName} -> programName) (\s@DeleteProgram' {} a -> s {programName = a} :: DeleteProgram)

instance Core.AWSRequest DeleteProgram where
  type
    AWSResponse DeleteProgram =
      DeleteProgramResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProgramResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProgram where
  hashWithSalt salt' DeleteProgram' {..} =
    salt' `Prelude.hashWithSalt` programName
      `Prelude.hashWithSalt` channelName

instance Prelude.NFData DeleteProgram where
  rnf DeleteProgram' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf programName

instance Core.ToHeaders DeleteProgram where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteProgram where
  toPath DeleteProgram' {..} =
    Prelude.mconcat
      [ "/channel/",
        Core.toBS channelName,
        "/program/",
        Core.toBS programName
      ]

instance Core.ToQuery DeleteProgram where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProgramResponse' smart constructor.
data DeleteProgramResponse = DeleteProgramResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProgramResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProgramResponse_httpStatus' - The response's http status code.
newDeleteProgramResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProgramResponse
newDeleteProgramResponse pHttpStatus_ =
  DeleteProgramResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteProgramResponse_httpStatus :: Lens.Lens' DeleteProgramResponse Prelude.Int
deleteProgramResponse_httpStatus = Lens.lens (\DeleteProgramResponse' {httpStatus} -> httpStatus) (\s@DeleteProgramResponse' {} a -> s {httpStatus = a} :: DeleteProgramResponse)

instance Prelude.NFData DeleteProgramResponse where
  rnf DeleteProgramResponse' {..} =
    Prelude.rnf httpStatus
