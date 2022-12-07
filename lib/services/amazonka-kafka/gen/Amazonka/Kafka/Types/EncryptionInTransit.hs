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
-- Module      : Amazonka.Kafka.Types.EncryptionInTransit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.EncryptionInTransit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.ClientBroker
import qualified Amazonka.Prelude as Prelude

-- | The settings for encrypting data in transit.
--
-- /See:/ 'newEncryptionInTransit' smart constructor.
data EncryptionInTransit = EncryptionInTransit'
  { -- | When set to true, it indicates that data communication among the broker
    -- nodes of the cluster is encrypted. When set to false, the communication
    -- happens in plaintext.
    --
    -- The default value is true.
    inCluster :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the encryption setting for data in transit between clients and
    -- brokers. The following are the possible values.
    --
    -- TLS means that client-broker communication is enabled with TLS only.
    --
    -- TLS_PLAINTEXT means that client-broker communication is enabled for both
    -- TLS-encrypted, as well as plaintext data.
    --
    -- PLAINTEXT means that client-broker communication is enabled in plaintext
    -- only.
    --
    -- The default value is TLS_PLAINTEXT.
    clientBroker :: Prelude.Maybe ClientBroker
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EncryptionInTransit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inCluster', 'encryptionInTransit_inCluster' - When set to true, it indicates that data communication among the broker
-- nodes of the cluster is encrypted. When set to false, the communication
-- happens in plaintext.
--
-- The default value is true.
--
-- 'clientBroker', 'encryptionInTransit_clientBroker' - Indicates the encryption setting for data in transit between clients and
-- brokers. The following are the possible values.
--
-- TLS means that client-broker communication is enabled with TLS only.
--
-- TLS_PLAINTEXT means that client-broker communication is enabled for both
-- TLS-encrypted, as well as plaintext data.
--
-- PLAINTEXT means that client-broker communication is enabled in plaintext
-- only.
--
-- The default value is TLS_PLAINTEXT.
newEncryptionInTransit ::
  EncryptionInTransit
newEncryptionInTransit =
  EncryptionInTransit'
    { inCluster = Prelude.Nothing,
      clientBroker = Prelude.Nothing
    }

-- | When set to true, it indicates that data communication among the broker
-- nodes of the cluster is encrypted. When set to false, the communication
-- happens in plaintext.
--
-- The default value is true.
encryptionInTransit_inCluster :: Lens.Lens' EncryptionInTransit (Prelude.Maybe Prelude.Bool)
encryptionInTransit_inCluster = Lens.lens (\EncryptionInTransit' {inCluster} -> inCluster) (\s@EncryptionInTransit' {} a -> s {inCluster = a} :: EncryptionInTransit)

-- | Indicates the encryption setting for data in transit between clients and
-- brokers. The following are the possible values.
--
-- TLS means that client-broker communication is enabled with TLS only.
--
-- TLS_PLAINTEXT means that client-broker communication is enabled for both
-- TLS-encrypted, as well as plaintext data.
--
-- PLAINTEXT means that client-broker communication is enabled in plaintext
-- only.
--
-- The default value is TLS_PLAINTEXT.
encryptionInTransit_clientBroker :: Lens.Lens' EncryptionInTransit (Prelude.Maybe ClientBroker)
encryptionInTransit_clientBroker = Lens.lens (\EncryptionInTransit' {clientBroker} -> clientBroker) (\s@EncryptionInTransit' {} a -> s {clientBroker = a} :: EncryptionInTransit)

instance Data.FromJSON EncryptionInTransit where
  parseJSON =
    Data.withObject
      "EncryptionInTransit"
      ( \x ->
          EncryptionInTransit'
            Prelude.<$> (x Data..:? "inCluster")
            Prelude.<*> (x Data..:? "clientBroker")
      )

instance Prelude.Hashable EncryptionInTransit where
  hashWithSalt _salt EncryptionInTransit' {..} =
    _salt `Prelude.hashWithSalt` inCluster
      `Prelude.hashWithSalt` clientBroker

instance Prelude.NFData EncryptionInTransit where
  rnf EncryptionInTransit' {..} =
    Prelude.rnf inCluster
      `Prelude.seq` Prelude.rnf clientBroker

instance Data.ToJSON EncryptionInTransit where
  toJSON EncryptionInTransit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inCluster" Data..=) Prelude.<$> inCluster,
            ("clientBroker" Data..=) Prelude.<$> clientBroker
          ]
      )
