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
-- Module      : Amazonka.MacieV2.DisassociateMember
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an Amazon Macie administrator account from a member
-- account.
module Amazonka.MacieV2.DisassociateMember
  ( -- * Creating a Request
    DisassociateMember (..),
    newDisassociateMember,

    -- * Request Lenses
    disassociateMember_id,

    -- * Destructuring the Response
    DisassociateMemberResponse (..),
    newDisassociateMemberResponse,

    -- * Response Lenses
    disassociateMemberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateMember' smart constructor.
data DisassociateMember = DisassociateMember'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateMember_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newDisassociateMember ::
  -- | 'id'
  Prelude.Text ->
  DisassociateMember
newDisassociateMember pId_ =
  DisassociateMember' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
disassociateMember_id :: Lens.Lens' DisassociateMember Prelude.Text
disassociateMember_id = Lens.lens (\DisassociateMember' {id} -> id) (\s@DisassociateMember' {} a -> s {id = a} :: DisassociateMember)

instance Core.AWSRequest DisassociateMember where
  type
    AWSResponse DisassociateMember =
      DisassociateMemberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateMemberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateMember where
  hashWithSalt _salt DisassociateMember' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DisassociateMember where
  rnf DisassociateMember' {..} = Prelude.rnf id

instance Core.ToHeaders DisassociateMember where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisassociateMember where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath DisassociateMember where
  toPath DisassociateMember' {..} =
    Prelude.mconcat
      ["/members/disassociate/", Core.toBS id]

instance Core.ToQuery DisassociateMember where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateMemberResponse' smart constructor.
data DisassociateMemberResponse = DisassociateMemberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateMemberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateMemberResponse_httpStatus' - The response's http status code.
newDisassociateMemberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateMemberResponse
newDisassociateMemberResponse pHttpStatus_ =
  DisassociateMemberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateMemberResponse_httpStatus :: Lens.Lens' DisassociateMemberResponse Prelude.Int
disassociateMemberResponse_httpStatus = Lens.lens (\DisassociateMemberResponse' {httpStatus} -> httpStatus) (\s@DisassociateMemberResponse' {} a -> s {httpStatus = a} :: DisassociateMemberResponse)

instance Prelude.NFData DisassociateMemberResponse where
  rnf DisassociateMemberResponse' {..} =
    Prelude.rnf httpStatus
