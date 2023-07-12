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
-- Module      : Amazonka.Lightsail.GetKeyPair
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific key pair.
module Amazonka.Lightsail.GetKeyPair
  ( -- * Creating a Request
    GetKeyPair (..),
    newGetKeyPair,

    -- * Request Lenses
    getKeyPair_keyPairName,

    -- * Destructuring the Response
    GetKeyPairResponse (..),
    newGetKeyPairResponse,

    -- * Response Lenses
    getKeyPairResponse_keyPair,
    getKeyPairResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKeyPair' smart constructor.
data GetKeyPair = GetKeyPair'
  { -- | The name of the key pair for which you are requesting information.
    keyPairName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyPair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPairName', 'getKeyPair_keyPairName' - The name of the key pair for which you are requesting information.
newGetKeyPair ::
  -- | 'keyPairName'
  Prelude.Text ->
  GetKeyPair
newGetKeyPair pKeyPairName_ =
  GetKeyPair' {keyPairName = pKeyPairName_}

-- | The name of the key pair for which you are requesting information.
getKeyPair_keyPairName :: Lens.Lens' GetKeyPair Prelude.Text
getKeyPair_keyPairName = Lens.lens (\GetKeyPair' {keyPairName} -> keyPairName) (\s@GetKeyPair' {} a -> s {keyPairName = a} :: GetKeyPair)

instance Core.AWSRequest GetKeyPair where
  type AWSResponse GetKeyPair = GetKeyPairResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPairResponse'
            Prelude.<$> (x Data..?> "keyPair")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKeyPair where
  hashWithSalt _salt GetKeyPair' {..} =
    _salt `Prelude.hashWithSalt` keyPairName

instance Prelude.NFData GetKeyPair where
  rnf GetKeyPair' {..} = Prelude.rnf keyPairName

instance Data.ToHeaders GetKeyPair where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetKeyPair" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetKeyPair where
  toJSON GetKeyPair' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("keyPairName" Data..= keyPairName)]
      )

instance Data.ToPath GetKeyPair where
  toPath = Prelude.const "/"

instance Data.ToQuery GetKeyPair where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetKeyPairResponse' smart constructor.
data GetKeyPairResponse = GetKeyPairResponse'
  { -- | An array of key-value pairs containing information about the key pair.
    keyPair :: Prelude.Maybe KeyPair,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKeyPairResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPair', 'getKeyPairResponse_keyPair' - An array of key-value pairs containing information about the key pair.
--
-- 'httpStatus', 'getKeyPairResponse_httpStatus' - The response's http status code.
newGetKeyPairResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKeyPairResponse
newGetKeyPairResponse pHttpStatus_ =
  GetKeyPairResponse'
    { keyPair = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the key pair.
getKeyPairResponse_keyPair :: Lens.Lens' GetKeyPairResponse (Prelude.Maybe KeyPair)
getKeyPairResponse_keyPair = Lens.lens (\GetKeyPairResponse' {keyPair} -> keyPair) (\s@GetKeyPairResponse' {} a -> s {keyPair = a} :: GetKeyPairResponse)

-- | The response's http status code.
getKeyPairResponse_httpStatus :: Lens.Lens' GetKeyPairResponse Prelude.Int
getKeyPairResponse_httpStatus = Lens.lens (\GetKeyPairResponse' {httpStatus} -> httpStatus) (\s@GetKeyPairResponse' {} a -> s {httpStatus = a} :: GetKeyPairResponse)

instance Prelude.NFData GetKeyPairResponse where
  rnf GetKeyPairResponse' {..} =
    Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf httpStatus
