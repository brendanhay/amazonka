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
-- Module      : Network.AWS.Lightsail.GetKeyPair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific key pair.
module Network.AWS.Lightsail.GetKeyPair
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetKeyPair' smart constructor.
data GetKeyPair = GetKeyPair'
  { -- | The name of the key pair for which you are requesting information.
    keyPairName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetKeyPair
newGetKeyPair pKeyPairName_ =
  GetKeyPair' {keyPairName = pKeyPairName_}

-- | The name of the key pair for which you are requesting information.
getKeyPair_keyPairName :: Lens.Lens' GetKeyPair Core.Text
getKeyPair_keyPairName = Lens.lens (\GetKeyPair' {keyPairName} -> keyPairName) (\s@GetKeyPair' {} a -> s {keyPairName = a} :: GetKeyPair)

instance Core.AWSRequest GetKeyPair where
  type AWSResponse GetKeyPair = GetKeyPairResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKeyPairResponse'
            Core.<$> (x Core..?> "keyPair")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetKeyPair

instance Core.NFData GetKeyPair

instance Core.ToHeaders GetKeyPair where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.GetKeyPair" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetKeyPair where
  toJSON GetKeyPair' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("keyPairName" Core..= keyPairName)]
      )

instance Core.ToPath GetKeyPair where
  toPath = Core.const "/"

instance Core.ToQuery GetKeyPair where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetKeyPairResponse' smart constructor.
data GetKeyPairResponse = GetKeyPairResponse'
  { -- | An array of key-value pairs containing information about the key pair.
    keyPair :: Core.Maybe KeyPair,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetKeyPairResponse
newGetKeyPairResponse pHttpStatus_ =
  GetKeyPairResponse'
    { keyPair = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the key pair.
getKeyPairResponse_keyPair :: Lens.Lens' GetKeyPairResponse (Core.Maybe KeyPair)
getKeyPairResponse_keyPair = Lens.lens (\GetKeyPairResponse' {keyPair} -> keyPair) (\s@GetKeyPairResponse' {} a -> s {keyPair = a} :: GetKeyPairResponse)

-- | The response's http status code.
getKeyPairResponse_httpStatus :: Lens.Lens' GetKeyPairResponse Core.Int
getKeyPairResponse_httpStatus = Lens.lens (\GetKeyPairResponse' {httpStatus} -> httpStatus) (\s@GetKeyPairResponse' {} a -> s {httpStatus = a} :: GetKeyPairResponse)

instance Core.NFData GetKeyPairResponse
