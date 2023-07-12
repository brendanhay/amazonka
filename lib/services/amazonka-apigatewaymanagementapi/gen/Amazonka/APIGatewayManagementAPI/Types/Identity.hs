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
-- Module      : Amazonka.APIGatewayManagementAPI.Types.Identity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGatewayManagementAPI.Types.Identity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | The source IP address of the TCP connection making the request to API
    -- Gateway.
    sourceIp :: Prelude.Text,
    -- | The User Agent of the API caller.
    userAgent :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Identity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceIp', 'identity_sourceIp' - The source IP address of the TCP connection making the request to API
-- Gateway.
--
-- 'userAgent', 'identity_userAgent' - The User Agent of the API caller.
newIdentity ::
  -- | 'sourceIp'
  Prelude.Text ->
  -- | 'userAgent'
  Prelude.Text ->
  Identity
newIdentity pSourceIp_ pUserAgent_ =
  Identity'
    { sourceIp = pSourceIp_,
      userAgent = pUserAgent_
    }

-- | The source IP address of the TCP connection making the request to API
-- Gateway.
identity_sourceIp :: Lens.Lens' Identity Prelude.Text
identity_sourceIp = Lens.lens (\Identity' {sourceIp} -> sourceIp) (\s@Identity' {} a -> s {sourceIp = a} :: Identity)

-- | The User Agent of the API caller.
identity_userAgent :: Lens.Lens' Identity Prelude.Text
identity_userAgent = Lens.lens (\Identity' {userAgent} -> userAgent) (\s@Identity' {} a -> s {userAgent = a} :: Identity)

instance Data.FromJSON Identity where
  parseJSON =
    Data.withObject
      "Identity"
      ( \x ->
          Identity'
            Prelude.<$> (x Data..: "sourceIp")
            Prelude.<*> (x Data..: "userAgent")
      )

instance Prelude.Hashable Identity where
  hashWithSalt _salt Identity' {..} =
    _salt
      `Prelude.hashWithSalt` sourceIp
      `Prelude.hashWithSalt` userAgent

instance Prelude.NFData Identity where
  rnf Identity' {..} =
    Prelude.rnf sourceIp
      `Prelude.seq` Prelude.rnf userAgent
