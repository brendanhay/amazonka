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
-- Module      : Amazonka.IVS.GetPlaybackKeyPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a specified playback authorization key pair and returns the @arn@
-- and @fingerprint@. The @privateKey@ held by the caller can be used to
-- generate viewer authorization tokens, to grant viewers access to private
-- channels. For more information, see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/private-channels.html Setting Up Private Channels>
-- in the /Amazon IVS User Guide/.
module Amazonka.IVS.GetPlaybackKeyPair
  ( -- * Creating a Request
    GetPlaybackKeyPair (..),
    newGetPlaybackKeyPair,

    -- * Request Lenses
    getPlaybackKeyPair_arn,

    -- * Destructuring the Response
    GetPlaybackKeyPairResponse (..),
    newGetPlaybackKeyPairResponse,

    -- * Response Lenses
    getPlaybackKeyPairResponse_keyPair,
    getPlaybackKeyPairResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPlaybackKeyPair' smart constructor.
data GetPlaybackKeyPair = GetPlaybackKeyPair'
  { -- | ARN of the key pair to be returned.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlaybackKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getPlaybackKeyPair_arn' - ARN of the key pair to be returned.
newGetPlaybackKeyPair ::
  -- | 'arn'
  Prelude.Text ->
  GetPlaybackKeyPair
newGetPlaybackKeyPair pArn_ =
  GetPlaybackKeyPair' {arn = pArn_}

-- | ARN of the key pair to be returned.
getPlaybackKeyPair_arn :: Lens.Lens' GetPlaybackKeyPair Prelude.Text
getPlaybackKeyPair_arn = Lens.lens (\GetPlaybackKeyPair' {arn} -> arn) (\s@GetPlaybackKeyPair' {} a -> s {arn = a} :: GetPlaybackKeyPair)

instance Core.AWSRequest GetPlaybackKeyPair where
  type
    AWSResponse GetPlaybackKeyPair =
      GetPlaybackKeyPairResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPlaybackKeyPairResponse'
            Prelude.<$> (x Data..?> "keyPair")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPlaybackKeyPair where
  hashWithSalt _salt GetPlaybackKeyPair' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetPlaybackKeyPair where
  rnf GetPlaybackKeyPair' {..} = Prelude.rnf arn

instance Data.ToHeaders GetPlaybackKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPlaybackKeyPair where
  toJSON GetPlaybackKeyPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetPlaybackKeyPair where
  toPath = Prelude.const "/GetPlaybackKeyPair"

instance Data.ToQuery GetPlaybackKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPlaybackKeyPairResponse' smart constructor.
data GetPlaybackKeyPairResponse = GetPlaybackKeyPairResponse'
  { keyPair :: Prelude.Maybe PlaybackKeyPair,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPlaybackKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPair', 'getPlaybackKeyPairResponse_keyPair' -
--
-- 'httpStatus', 'getPlaybackKeyPairResponse_httpStatus' - The response's http status code.
newGetPlaybackKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPlaybackKeyPairResponse
newGetPlaybackKeyPairResponse pHttpStatus_ =
  GetPlaybackKeyPairResponse'
    { keyPair =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

getPlaybackKeyPairResponse_keyPair :: Lens.Lens' GetPlaybackKeyPairResponse (Prelude.Maybe PlaybackKeyPair)
getPlaybackKeyPairResponse_keyPair = Lens.lens (\GetPlaybackKeyPairResponse' {keyPair} -> keyPair) (\s@GetPlaybackKeyPairResponse' {} a -> s {keyPair = a} :: GetPlaybackKeyPairResponse)

-- | The response's http status code.
getPlaybackKeyPairResponse_httpStatus :: Lens.Lens' GetPlaybackKeyPairResponse Prelude.Int
getPlaybackKeyPairResponse_httpStatus = Lens.lens (\GetPlaybackKeyPairResponse' {httpStatus} -> httpStatus) (\s@GetPlaybackKeyPairResponse' {} a -> s {httpStatus = a} :: GetPlaybackKeyPairResponse)

instance Prelude.NFData GetPlaybackKeyPairResponse where
  rnf GetPlaybackKeyPairResponse' {..} =
    Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf httpStatus
