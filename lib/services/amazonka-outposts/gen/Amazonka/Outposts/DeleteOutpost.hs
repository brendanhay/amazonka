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
-- Module      : Amazonka.Outposts.DeleteOutpost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Outpost.
module Amazonka.Outposts.DeleteOutpost
  ( -- * Creating a Request
    DeleteOutpost (..),
    newDeleteOutpost,

    -- * Request Lenses
    deleteOutpost_outpostId,

    -- * Destructuring the Response
    DeleteOutpostResponse (..),
    newDeleteOutpostResponse,

    -- * Response Lenses
    deleteOutpostResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteOutpost' smart constructor.
data DeleteOutpost = DeleteOutpost'
  { -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOutpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpostId', 'deleteOutpost_outpostId' - The ID or the Amazon Resource Name (ARN) of the Outpost.
newDeleteOutpost ::
  -- | 'outpostId'
  Prelude.Text ->
  DeleteOutpost
newDeleteOutpost pOutpostId_ =
  DeleteOutpost' {outpostId = pOutpostId_}

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
deleteOutpost_outpostId :: Lens.Lens' DeleteOutpost Prelude.Text
deleteOutpost_outpostId = Lens.lens (\DeleteOutpost' {outpostId} -> outpostId) (\s@DeleteOutpost' {} a -> s {outpostId = a} :: DeleteOutpost)

instance Core.AWSRequest DeleteOutpost where
  type
    AWSResponse DeleteOutpost =
      DeleteOutpostResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteOutpostResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteOutpost where
  hashWithSalt _salt DeleteOutpost' {..} =
    _salt `Prelude.hashWithSalt` outpostId

instance Prelude.NFData DeleteOutpost where
  rnf DeleteOutpost' {..} = Prelude.rnf outpostId

instance Core.ToHeaders DeleteOutpost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteOutpost where
  toPath DeleteOutpost' {..} =
    Prelude.mconcat ["/outposts/", Core.toBS outpostId]

instance Core.ToQuery DeleteOutpost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOutpostResponse' smart constructor.
data DeleteOutpostResponse = DeleteOutpostResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteOutpostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteOutpostResponse_httpStatus' - The response's http status code.
newDeleteOutpostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteOutpostResponse
newDeleteOutpostResponse pHttpStatus_ =
  DeleteOutpostResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteOutpostResponse_httpStatus :: Lens.Lens' DeleteOutpostResponse Prelude.Int
deleteOutpostResponse_httpStatus = Lens.lens (\DeleteOutpostResponse' {httpStatus} -> httpStatus) (\s@DeleteOutpostResponse' {} a -> s {httpStatus = a} :: DeleteOutpostResponse)

instance Prelude.NFData DeleteOutpostResponse where
  rnf DeleteOutpostResponse' {..} =
    Prelude.rnf httpStatus
