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
-- Module      : Amazonka.NetworkManager.Types.BgpOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.BgpOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the BGP options.
--
-- /See:/ 'newBgpOptions' smart constructor.
data BgpOptions = BgpOptions'
  { -- | The Peer ASN of the BGP.
    peerAsn :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BgpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peerAsn', 'bgpOptions_peerAsn' - The Peer ASN of the BGP.
newBgpOptions ::
  BgpOptions
newBgpOptions =
  BgpOptions' {peerAsn = Prelude.Nothing}

-- | The Peer ASN of the BGP.
bgpOptions_peerAsn :: Lens.Lens' BgpOptions (Prelude.Maybe Prelude.Integer)
bgpOptions_peerAsn = Lens.lens (\BgpOptions' {peerAsn} -> peerAsn) (\s@BgpOptions' {} a -> s {peerAsn = a} :: BgpOptions)

instance Prelude.Hashable BgpOptions where
  hashWithSalt _salt BgpOptions' {..} =
    _salt `Prelude.hashWithSalt` peerAsn

instance Prelude.NFData BgpOptions where
  rnf BgpOptions' {..} = Prelude.rnf peerAsn

instance Data.ToJSON BgpOptions where
  toJSON BgpOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [("PeerAsn" Data..=) Prelude.<$> peerAsn]
      )
