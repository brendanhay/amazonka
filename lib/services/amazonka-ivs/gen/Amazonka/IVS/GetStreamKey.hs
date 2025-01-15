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
-- Module      : Amazonka.IVS.GetStreamKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets stream-key information for a specified ARN.
module Amazonka.IVS.GetStreamKey
  ( -- * Creating a Request
    GetStreamKey (..),
    newGetStreamKey,

    -- * Request Lenses
    getStreamKey_arn,

    -- * Destructuring the Response
    GetStreamKeyResponse (..),
    newGetStreamKeyResponse,

    -- * Response Lenses
    getStreamKeyResponse_streamKey,
    getStreamKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStreamKey' smart constructor.
data GetStreamKey = GetStreamKey'
  { -- | ARN for the stream key to be retrieved.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getStreamKey_arn' - ARN for the stream key to be retrieved.
newGetStreamKey ::
  -- | 'arn'
  Prelude.Text ->
  GetStreamKey
newGetStreamKey pArn_ = GetStreamKey' {arn = pArn_}

-- | ARN for the stream key to be retrieved.
getStreamKey_arn :: Lens.Lens' GetStreamKey Prelude.Text
getStreamKey_arn = Lens.lens (\GetStreamKey' {arn} -> arn) (\s@GetStreamKey' {} a -> s {arn = a} :: GetStreamKey)

instance Core.AWSRequest GetStreamKey where
  type AWSResponse GetStreamKey = GetStreamKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStreamKeyResponse'
            Prelude.<$> (x Data..?> "streamKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStreamKey where
  hashWithSalt _salt GetStreamKey' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetStreamKey where
  rnf GetStreamKey' {..} = Prelude.rnf arn

instance Data.ToHeaders GetStreamKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStreamKey where
  toJSON GetStreamKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetStreamKey where
  toPath = Prelude.const "/GetStreamKey"

instance Data.ToQuery GetStreamKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStreamKeyResponse' smart constructor.
data GetStreamKeyResponse = GetStreamKeyResponse'
  { streamKey :: Prelude.Maybe StreamKey,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStreamKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamKey', 'getStreamKeyResponse_streamKey' -
--
-- 'httpStatus', 'getStreamKeyResponse_httpStatus' - The response's http status code.
newGetStreamKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStreamKeyResponse
newGetStreamKeyResponse pHttpStatus_ =
  GetStreamKeyResponse'
    { streamKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

getStreamKeyResponse_streamKey :: Lens.Lens' GetStreamKeyResponse (Prelude.Maybe StreamKey)
getStreamKeyResponse_streamKey = Lens.lens (\GetStreamKeyResponse' {streamKey} -> streamKey) (\s@GetStreamKeyResponse' {} a -> s {streamKey = a} :: GetStreamKeyResponse)

-- | The response's http status code.
getStreamKeyResponse_httpStatus :: Lens.Lens' GetStreamKeyResponse Prelude.Int
getStreamKeyResponse_httpStatus = Lens.lens (\GetStreamKeyResponse' {httpStatus} -> httpStatus) (\s@GetStreamKeyResponse' {} a -> s {httpStatus = a} :: GetStreamKeyResponse)

instance Prelude.NFData GetStreamKeyResponse where
  rnf GetStreamKeyResponse' {..} =
    Prelude.rnf streamKey `Prelude.seq`
      Prelude.rnf httpStatus
