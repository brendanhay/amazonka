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
-- Module      : Amazonka.AppMesh.Types.ClientPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.ClientPolicy where

import Amazonka.AppMesh.Types.ClientPolicyTls
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a client policy.
--
-- /See:/ 'newClientPolicy' smart constructor.
data ClientPolicy = ClientPolicy'
  { -- | A reference to an object that represents a Transport Layer Security
    -- (TLS) client policy.
    tls :: Prelude.Maybe ClientPolicyTls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tls', 'clientPolicy_tls' - A reference to an object that represents a Transport Layer Security
-- (TLS) client policy.
newClientPolicy ::
  ClientPolicy
newClientPolicy =
  ClientPolicy' {tls = Prelude.Nothing}

-- | A reference to an object that represents a Transport Layer Security
-- (TLS) client policy.
clientPolicy_tls :: Lens.Lens' ClientPolicy (Prelude.Maybe ClientPolicyTls)
clientPolicy_tls = Lens.lens (\ClientPolicy' {tls} -> tls) (\s@ClientPolicy' {} a -> s {tls = a} :: ClientPolicy)

instance Data.FromJSON ClientPolicy where
  parseJSON =
    Data.withObject
      "ClientPolicy"
      (\x -> ClientPolicy' Prelude.<$> (x Data..:? "tls"))

instance Prelude.Hashable ClientPolicy where
  hashWithSalt _salt ClientPolicy' {..} =
    _salt `Prelude.hashWithSalt` tls

instance Prelude.NFData ClientPolicy where
  rnf ClientPolicy' {..} = Prelude.rnf tls

instance Data.ToJSON ClientPolicy where
  toJSON ClientPolicy' {..} =
    Data.object
      (Prelude.catMaybes [("tls" Data..=) Prelude.<$> tls])
