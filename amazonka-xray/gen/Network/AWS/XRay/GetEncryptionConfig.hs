{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.XRay.GetEncryptionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current encryption configuration for X-Ray data.
module Network.AWS.XRay.GetEncryptionConfig
  ( -- * Creating a Request
    GetEncryptionConfig (..),
    newGetEncryptionConfig,

    -- * Destructuring the Response
    GetEncryptionConfigResponse (..),
    newGetEncryptionConfigResponse,

    -- * Response Lenses
    getEncryptionConfigResponse_encryptionConfig,
    getEncryptionConfigResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetEncryptionConfig' smart constructor.
data GetEncryptionConfig = GetEncryptionConfig'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEncryptionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetEncryptionConfig ::
  GetEncryptionConfig
newGetEncryptionConfig = GetEncryptionConfig'

instance Prelude.AWSRequest GetEncryptionConfig where
  type
    Rs GetEncryptionConfig =
      GetEncryptionConfigResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEncryptionConfigResponse'
            Prelude.<$> (x Prelude..?> "EncryptionConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEncryptionConfig

instance Prelude.NFData GetEncryptionConfig

instance Prelude.ToHeaders GetEncryptionConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON GetEncryptionConfig where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetEncryptionConfig where
  toPath = Prelude.const "/EncryptionConfig"

instance Prelude.ToQuery GetEncryptionConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEncryptionConfigResponse' smart constructor.
data GetEncryptionConfigResponse = GetEncryptionConfigResponse'
  { -- | The encryption configuration document.
    encryptionConfig :: Prelude.Maybe EncryptionConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetEncryptionConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfig', 'getEncryptionConfigResponse_encryptionConfig' - The encryption configuration document.
--
-- 'httpStatus', 'getEncryptionConfigResponse_httpStatus' - The response's http status code.
newGetEncryptionConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEncryptionConfigResponse
newGetEncryptionConfigResponse pHttpStatus_ =
  GetEncryptionConfigResponse'
    { encryptionConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The encryption configuration document.
getEncryptionConfigResponse_encryptionConfig :: Lens.Lens' GetEncryptionConfigResponse (Prelude.Maybe EncryptionConfig)
getEncryptionConfigResponse_encryptionConfig = Lens.lens (\GetEncryptionConfigResponse' {encryptionConfig} -> encryptionConfig) (\s@GetEncryptionConfigResponse' {} a -> s {encryptionConfig = a} :: GetEncryptionConfigResponse)

-- | The response's http status code.
getEncryptionConfigResponse_httpStatus :: Lens.Lens' GetEncryptionConfigResponse Prelude.Int
getEncryptionConfigResponse_httpStatus = Lens.lens (\GetEncryptionConfigResponse' {httpStatus} -> httpStatus) (\s@GetEncryptionConfigResponse' {} a -> s {httpStatus = a} :: GetEncryptionConfigResponse)

instance Prelude.NFData GetEncryptionConfigResponse
