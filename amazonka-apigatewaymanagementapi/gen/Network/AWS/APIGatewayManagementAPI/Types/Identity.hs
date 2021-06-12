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
-- Module      : Network.AWS.APIGatewayManagementAPI.Types.Identity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGatewayManagementAPI.Types.Identity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newIdentity' smart constructor.
data Identity = Identity'
  { -- | The source IP address of the TCP connection making the request to API
    -- Gateway.
    sourceIp :: Core.Text,
    -- | The User Agent of the API caller.
    userAgent :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'userAgent'
  Core.Text ->
  Identity
newIdentity pSourceIp_ pUserAgent_ =
  Identity'
    { sourceIp = pSourceIp_,
      userAgent = pUserAgent_
    }

-- | The source IP address of the TCP connection making the request to API
-- Gateway.
identity_sourceIp :: Lens.Lens' Identity Core.Text
identity_sourceIp = Lens.lens (\Identity' {sourceIp} -> sourceIp) (\s@Identity' {} a -> s {sourceIp = a} :: Identity)

-- | The User Agent of the API caller.
identity_userAgent :: Lens.Lens' Identity Core.Text
identity_userAgent = Lens.lens (\Identity' {userAgent} -> userAgent) (\s@Identity' {} a -> s {userAgent = a} :: Identity)

instance Core.FromJSON Identity where
  parseJSON =
    Core.withObject
      "Identity"
      ( \x ->
          Identity'
            Core.<$> (x Core..: "sourceIp")
            Core.<*> (x Core..: "userAgent")
      )

instance Core.Hashable Identity

instance Core.NFData Identity
