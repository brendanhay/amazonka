{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.RedirectAllRequestsTo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RedirectAllRequestsTo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.Protocol

-- | Specifies the redirect behavior of all requests to a website endpoint of
-- an Amazon S3 bucket.
--
-- /See:/ 'newRedirectAllRequestsTo' smart constructor.
data RedirectAllRequestsTo = RedirectAllRequestsTo'
  { -- | Protocol to use when redirecting requests. The default is the protocol
    -- that is used in the original request.
    protocol :: Prelude.Maybe Protocol,
    -- | Name of the host where requests are redirected.
    hostName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RedirectAllRequestsTo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'redirectAllRequestsTo_protocol' - Protocol to use when redirecting requests. The default is the protocol
-- that is used in the original request.
--
-- 'hostName', 'redirectAllRequestsTo_hostName' - Name of the host where requests are redirected.
newRedirectAllRequestsTo ::
  -- | 'hostName'
  Prelude.Text ->
  RedirectAllRequestsTo
newRedirectAllRequestsTo pHostName_ =
  RedirectAllRequestsTo'
    { protocol = Prelude.Nothing,
      hostName = pHostName_
    }

-- | Protocol to use when redirecting requests. The default is the protocol
-- that is used in the original request.
redirectAllRequestsTo_protocol :: Lens.Lens' RedirectAllRequestsTo (Prelude.Maybe Protocol)
redirectAllRequestsTo_protocol = Lens.lens (\RedirectAllRequestsTo' {protocol} -> protocol) (\s@RedirectAllRequestsTo' {} a -> s {protocol = a} :: RedirectAllRequestsTo)

-- | Name of the host where requests are redirected.
redirectAllRequestsTo_hostName :: Lens.Lens' RedirectAllRequestsTo Prelude.Text
redirectAllRequestsTo_hostName = Lens.lens (\RedirectAllRequestsTo' {hostName} -> hostName) (\s@RedirectAllRequestsTo' {} a -> s {hostName = a} :: RedirectAllRequestsTo)

instance Prelude.FromXML RedirectAllRequestsTo where
  parseXML x =
    RedirectAllRequestsTo'
      Prelude.<$> (x Prelude..@? "Protocol")
      Prelude.<*> (x Prelude..@ "HostName")

instance Prelude.Hashable RedirectAllRequestsTo

instance Prelude.NFData RedirectAllRequestsTo

instance Prelude.ToXML RedirectAllRequestsTo where
  toXML RedirectAllRequestsTo' {..} =
    Prelude.mconcat
      [ "Protocol" Prelude.@= protocol,
        "HostName" Prelude.@= hostName
      ]
