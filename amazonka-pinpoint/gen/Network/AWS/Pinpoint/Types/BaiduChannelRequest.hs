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
-- Module      : Network.AWS.Pinpoint.Types.BaiduChannelRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduChannelRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the status and settings of the Baidu (Baidu Cloud Push)
-- channel for an application.
--
-- /See:/ 'newBaiduChannelRequest' smart constructor.
data BaiduChannelRequest = BaiduChannelRequest'
  { -- | Specifies whether to enable the Baidu channel for the application.
    enabled :: Core.Maybe Core.Bool,
    -- | The secret key that you received from the Baidu Cloud Push service to
    -- communicate with the service.
    secretKey :: Core.Text,
    -- | The API key that you received from the Baidu Cloud Push service to
    -- communicate with the service.
    apiKey :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'apiKey'
  Core.Text ->
  BaiduChannelRequest
newBaiduChannelRequest pSecretKey_ pApiKey_ =
  BaiduChannelRequest'
    { enabled = Core.Nothing,
      secretKey = pSecretKey_,
      apiKey = pApiKey_
    }

-- | Specifies whether to enable the Baidu channel for the application.
baiduChannelRequest_enabled :: Lens.Lens' BaiduChannelRequest (Core.Maybe Core.Bool)
baiduChannelRequest_enabled = Lens.lens (\BaiduChannelRequest' {enabled} -> enabled) (\s@BaiduChannelRequest' {} a -> s {enabled = a} :: BaiduChannelRequest)

-- | The secret key that you received from the Baidu Cloud Push service to
-- communicate with the service.
baiduChannelRequest_secretKey :: Lens.Lens' BaiduChannelRequest Core.Text
baiduChannelRequest_secretKey = Lens.lens (\BaiduChannelRequest' {secretKey} -> secretKey) (\s@BaiduChannelRequest' {} a -> s {secretKey = a} :: BaiduChannelRequest)

-- | The API key that you received from the Baidu Cloud Push service to
-- communicate with the service.
baiduChannelRequest_apiKey :: Lens.Lens' BaiduChannelRequest Core.Text
baiduChannelRequest_apiKey = Lens.lens (\BaiduChannelRequest' {apiKey} -> apiKey) (\s@BaiduChannelRequest' {} a -> s {apiKey = a} :: BaiduChannelRequest)

instance Core.Hashable BaiduChannelRequest

instance Core.NFData BaiduChannelRequest

instance Core.ToJSON BaiduChannelRequest where
  toJSON BaiduChannelRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            Core.Just ("SecretKey" Core..= secretKey),
            Core.Just ("ApiKey" Core..= apiKey)
          ]
      )
