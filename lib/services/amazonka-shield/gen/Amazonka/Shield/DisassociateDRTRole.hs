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
-- Module      : Amazonka.Shield.DisassociateDRTRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the Shield Response Team\'s (SRT) access to your Amazon Web
-- Services account.
module Amazonka.Shield.DisassociateDRTRole
  ( -- * Creating a Request
    DisassociateDRTRole (..),
    newDisassociateDRTRole,

    -- * Destructuring the Response
    DisassociateDRTRoleResponse (..),
    newDisassociateDRTRoleResponse,

    -- * Response Lenses
    disassociateDRTRoleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDisassociateDRTRole' smart constructor.
data DisassociateDRTRole = DisassociateDRTRole'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDRTRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateDRTRole ::
  DisassociateDRTRole
newDisassociateDRTRole = DisassociateDRTRole'

instance Core.AWSRequest DisassociateDRTRole where
  type
    AWSResponse DisassociateDRTRole =
      DisassociateDRTRoleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateDRTRoleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateDRTRole where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisassociateDRTRole where
  rnf _ = ()

instance Data.ToHeaders DisassociateDRTRole where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DisassociateDRTRole" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateDRTRole where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisassociateDRTRole where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateDRTRole where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateDRTRoleResponse' smart constructor.
data DisassociateDRTRoleResponse = DisassociateDRTRoleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateDRTRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateDRTRoleResponse_httpStatus' - The response's http status code.
newDisassociateDRTRoleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateDRTRoleResponse
newDisassociateDRTRoleResponse pHttpStatus_ =
  DisassociateDRTRoleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateDRTRoleResponse_httpStatus :: Lens.Lens' DisassociateDRTRoleResponse Prelude.Int
disassociateDRTRoleResponse_httpStatus = Lens.lens (\DisassociateDRTRoleResponse' {httpStatus} -> httpStatus) (\s@DisassociateDRTRoleResponse' {} a -> s {httpStatus = a} :: DisassociateDRTRoleResponse)

instance Prelude.NFData DisassociateDRTRoleResponse where
  rnf DisassociateDRTRoleResponse' {..} =
    Prelude.rnf httpStatus
