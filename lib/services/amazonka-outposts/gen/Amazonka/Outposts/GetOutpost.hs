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
-- Module      : Amazonka.Outposts.GetOutpost
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified Outpost.
module Amazonka.Outposts.GetOutpost
  ( -- * Creating a Request
    GetOutpost (..),
    newGetOutpost,

    -- * Request Lenses
    getOutpost_outpostId,

    -- * Destructuring the Response
    GetOutpostResponse (..),
    newGetOutpostResponse,

    -- * Response Lenses
    getOutpostResponse_outpost,
    getOutpostResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOutpost' smart constructor.
data GetOutpost = GetOutpost'
  { -- | The ID or the Amazon Resource Name (ARN) of the Outpost.
    outpostId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOutpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpostId', 'getOutpost_outpostId' - The ID or the Amazon Resource Name (ARN) of the Outpost.
newGetOutpost ::
  -- | 'outpostId'
  Prelude.Text ->
  GetOutpost
newGetOutpost pOutpostId_ =
  GetOutpost' {outpostId = pOutpostId_}

-- | The ID or the Amazon Resource Name (ARN) of the Outpost.
getOutpost_outpostId :: Lens.Lens' GetOutpost Prelude.Text
getOutpost_outpostId = Lens.lens (\GetOutpost' {outpostId} -> outpostId) (\s@GetOutpost' {} a -> s {outpostId = a} :: GetOutpost)

instance Core.AWSRequest GetOutpost where
  type AWSResponse GetOutpost = GetOutpostResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOutpostResponse'
            Prelude.<$> (x Data..?> "Outpost")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOutpost where
  hashWithSalt _salt GetOutpost' {..} =
    _salt `Prelude.hashWithSalt` outpostId

instance Prelude.NFData GetOutpost where
  rnf GetOutpost' {..} = Prelude.rnf outpostId

instance Data.ToHeaders GetOutpost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetOutpost where
  toPath GetOutpost' {..} =
    Prelude.mconcat ["/outposts/", Data.toBS outpostId]

instance Data.ToQuery GetOutpost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOutpostResponse' smart constructor.
data GetOutpostResponse = GetOutpostResponse'
  { outpost :: Prelude.Maybe Outpost,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOutpostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpost', 'getOutpostResponse_outpost' - Undocumented member.
--
-- 'httpStatus', 'getOutpostResponse_httpStatus' - The response's http status code.
newGetOutpostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOutpostResponse
newGetOutpostResponse pHttpStatus_ =
  GetOutpostResponse'
    { outpost = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getOutpostResponse_outpost :: Lens.Lens' GetOutpostResponse (Prelude.Maybe Outpost)
getOutpostResponse_outpost = Lens.lens (\GetOutpostResponse' {outpost} -> outpost) (\s@GetOutpostResponse' {} a -> s {outpost = a} :: GetOutpostResponse)

-- | The response's http status code.
getOutpostResponse_httpStatus :: Lens.Lens' GetOutpostResponse Prelude.Int
getOutpostResponse_httpStatus = Lens.lens (\GetOutpostResponse' {httpStatus} -> httpStatus) (\s@GetOutpostResponse' {} a -> s {httpStatus = a} :: GetOutpostResponse)

instance Prelude.NFData GetOutpostResponse where
  rnf GetOutpostResponse' {..} =
    Prelude.rnf outpost
      `Prelude.seq` Prelude.rnf httpStatus
