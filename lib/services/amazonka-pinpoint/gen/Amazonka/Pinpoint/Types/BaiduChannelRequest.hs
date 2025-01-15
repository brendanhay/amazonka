{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pinpoint.Types.BaiduChannelRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.BaiduChannelRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the status and settings of the Baidu (Baidu Cloud Push)
-- channel for an application.
--
-- /See:/ 'newBaiduChannelRequest' smart constructor.
data BaiduChannelRequest = BaiduChannelRequest'
  { -- | Specifies whether to enable the Baidu channel for the application.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The secret key that you received from the Baidu Cloud Push service to
    -- communicate with the service.
    secretKey :: Prelude.Text,
    -- | The API key that you received from the Baidu Cloud Push service to
    -- communicate with the service.
    apiKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaiduChannelRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'baiduChannelRequest_enabled' - Specifies whether to enable the Baidu channel for the application.
--
-- 'secretKey', 'baiduChannelRequest_secretKey' - The secret key that you received from the Baidu Cloud Push service to
-- communicate with the service.
--
-- 'apiKey', 'baiduChannelRequest_apiKey' - The API key that you received from the Baidu Cloud Push service to
-- communicate with the service.
newBaiduChannelRequest ::
  -- | 'secretKey'
  Prelude.Text ->
  -- | 'apiKey'
  Prelude.Text ->
  BaiduChannelRequest
newBaiduChannelRequest pSecretKey_ pApiKey_ =
  BaiduChannelRequest'
    { enabled = Prelude.Nothing,
      secretKey = pSecretKey_,
      apiKey = pApiKey_
    }

-- | Specifies whether to enable the Baidu channel for the application.
baiduChannelRequest_enabled :: Lens.Lens' BaiduChannelRequest (Prelude.Maybe Prelude.Bool)
baiduChannelRequest_enabled = Lens.lens (\BaiduChannelRequest' {enabled} -> enabled) (\s@BaiduChannelRequest' {} a -> s {enabled = a} :: BaiduChannelRequest)

-- | The secret key that you received from the Baidu Cloud Push service to
-- communicate with the service.
baiduChannelRequest_secretKey :: Lens.Lens' BaiduChannelRequest Prelude.Text
baiduChannelRequest_secretKey = Lens.lens (\BaiduChannelRequest' {secretKey} -> secretKey) (\s@BaiduChannelRequest' {} a -> s {secretKey = a} :: BaiduChannelRequest)

-- | The API key that you received from the Baidu Cloud Push service to
-- communicate with the service.
baiduChannelRequest_apiKey :: Lens.Lens' BaiduChannelRequest Prelude.Text
baiduChannelRequest_apiKey = Lens.lens (\BaiduChannelRequest' {apiKey} -> apiKey) (\s@BaiduChannelRequest' {} a -> s {apiKey = a} :: BaiduChannelRequest)

instance Prelude.Hashable BaiduChannelRequest where
  hashWithSalt _salt BaiduChannelRequest' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` secretKey
      `Prelude.hashWithSalt` apiKey

instance Prelude.NFData BaiduChannelRequest where
  rnf BaiduChannelRequest' {..} =
    Prelude.rnf enabled `Prelude.seq`
      Prelude.rnf secretKey `Prelude.seq`
        Prelude.rnf apiKey

instance Data.ToJSON BaiduChannelRequest where
  toJSON BaiduChannelRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Enabled" Data..=) Prelude.<$> enabled,
            Prelude.Just ("SecretKey" Data..= secretKey),
            Prelude.Just ("ApiKey" Data..= apiKey)
          ]
      )
