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
-- Module      : Network.AWS.XRay.Types.Http
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Http where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about an HTTP request.
--
-- /See:/ 'newHttp' smart constructor.
data Http = Http'
  { -- | The request method.
    httpMethod :: Core.Maybe Core.Text,
    -- | The request URL.
    httpURL :: Core.Maybe Core.Text,
    -- | The request\'s user agent string.
    userAgent :: Core.Maybe Core.Text,
    -- | The response status.
    httpStatus :: Core.Maybe Core.Int,
    -- | The IP address of the requestor.
    clientIp :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Http' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'http_httpMethod' - The request method.
--
-- 'httpURL', 'http_httpURL' - The request URL.
--
-- 'userAgent', 'http_userAgent' - The request\'s user agent string.
--
-- 'httpStatus', 'http_httpStatus' - The response status.
--
-- 'clientIp', 'http_clientIp' - The IP address of the requestor.
newHttp ::
  Http
newHttp =
  Http'
    { httpMethod = Core.Nothing,
      httpURL = Core.Nothing,
      userAgent = Core.Nothing,
      httpStatus = Core.Nothing,
      clientIp = Core.Nothing
    }

-- | The request method.
http_httpMethod :: Lens.Lens' Http (Core.Maybe Core.Text)
http_httpMethod = Lens.lens (\Http' {httpMethod} -> httpMethod) (\s@Http' {} a -> s {httpMethod = a} :: Http)

-- | The request URL.
http_httpURL :: Lens.Lens' Http (Core.Maybe Core.Text)
http_httpURL = Lens.lens (\Http' {httpURL} -> httpURL) (\s@Http' {} a -> s {httpURL = a} :: Http)

-- | The request\'s user agent string.
http_userAgent :: Lens.Lens' Http (Core.Maybe Core.Text)
http_userAgent = Lens.lens (\Http' {userAgent} -> userAgent) (\s@Http' {} a -> s {userAgent = a} :: Http)

-- | The response status.
http_httpStatus :: Lens.Lens' Http (Core.Maybe Core.Int)
http_httpStatus = Lens.lens (\Http' {httpStatus} -> httpStatus) (\s@Http' {} a -> s {httpStatus = a} :: Http)

-- | The IP address of the requestor.
http_clientIp :: Lens.Lens' Http (Core.Maybe Core.Text)
http_clientIp = Lens.lens (\Http' {clientIp} -> clientIp) (\s@Http' {} a -> s {clientIp = a} :: Http)

instance Core.FromJSON Http where
  parseJSON =
    Core.withObject
      "Http"
      ( \x ->
          Http'
            Core.<$> (x Core..:? "HttpMethod")
            Core.<*> (x Core..:? "HttpURL")
            Core.<*> (x Core..:? "UserAgent")
            Core.<*> (x Core..:? "HttpStatus")
            Core.<*> (x Core..:? "ClientIp")
      )

instance Core.Hashable Http

instance Core.NFData Http
