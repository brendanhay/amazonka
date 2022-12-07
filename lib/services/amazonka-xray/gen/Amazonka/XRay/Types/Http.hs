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
-- Module      : Amazonka.XRay.Types.Http
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.Http where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an HTTP request.
--
-- /See:/ 'newHttp' smart constructor.
data Http = Http'
  { -- | The request URL.
    httpURL :: Prelude.Maybe Prelude.Text,
    -- | The response status.
    httpStatus :: Prelude.Maybe Prelude.Int,
    -- | The request method.
    httpMethod :: Prelude.Maybe Prelude.Text,
    -- | The request\'s user agent string.
    userAgent :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the requestor.
    clientIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Http' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpURL', 'http_httpURL' - The request URL.
--
-- 'httpStatus', 'http_httpStatus' - The response status.
--
-- 'httpMethod', 'http_httpMethod' - The request method.
--
-- 'userAgent', 'http_userAgent' - The request\'s user agent string.
--
-- 'clientIp', 'http_clientIp' - The IP address of the requestor.
newHttp ::
  Http
newHttp =
  Http'
    { httpURL = Prelude.Nothing,
      httpStatus = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      userAgent = Prelude.Nothing,
      clientIp = Prelude.Nothing
    }

-- | The request URL.
http_httpURL :: Lens.Lens' Http (Prelude.Maybe Prelude.Text)
http_httpURL = Lens.lens (\Http' {httpURL} -> httpURL) (\s@Http' {} a -> s {httpURL = a} :: Http)

-- | The response status.
http_httpStatus :: Lens.Lens' Http (Prelude.Maybe Prelude.Int)
http_httpStatus = Lens.lens (\Http' {httpStatus} -> httpStatus) (\s@Http' {} a -> s {httpStatus = a} :: Http)

-- | The request method.
http_httpMethod :: Lens.Lens' Http (Prelude.Maybe Prelude.Text)
http_httpMethod = Lens.lens (\Http' {httpMethod} -> httpMethod) (\s@Http' {} a -> s {httpMethod = a} :: Http)

-- | The request\'s user agent string.
http_userAgent :: Lens.Lens' Http (Prelude.Maybe Prelude.Text)
http_userAgent = Lens.lens (\Http' {userAgent} -> userAgent) (\s@Http' {} a -> s {userAgent = a} :: Http)

-- | The IP address of the requestor.
http_clientIp :: Lens.Lens' Http (Prelude.Maybe Prelude.Text)
http_clientIp = Lens.lens (\Http' {clientIp} -> clientIp) (\s@Http' {} a -> s {clientIp = a} :: Http)

instance Data.FromJSON Http where
  parseJSON =
    Data.withObject
      "Http"
      ( \x ->
          Http'
            Prelude.<$> (x Data..:? "HttpURL")
            Prelude.<*> (x Data..:? "HttpStatus")
            Prelude.<*> (x Data..:? "HttpMethod")
            Prelude.<*> (x Data..:? "UserAgent")
            Prelude.<*> (x Data..:? "ClientIp")
      )

instance Prelude.Hashable Http where
  hashWithSalt _salt Http' {..} =
    _salt `Prelude.hashWithSalt` httpURL
      `Prelude.hashWithSalt` httpStatus
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` userAgent
      `Prelude.hashWithSalt` clientIp

instance Prelude.NFData Http where
  rnf Http' {..} =
    Prelude.rnf httpURL
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf userAgent
      `Prelude.seq` Prelude.rnf clientIp
