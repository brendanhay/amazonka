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
-- Module      : Amazonka.VPCLattice.Types.ListenerSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VPCLattice.Types.ListenerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VPCLattice.Types.ListenerProtocol

-- | Summary information about a listener.
--
-- /See:/ 'newListenerSummary' smart constructor.
data ListenerSummary = ListenerSummary'
  { -- | The Amazon Resource Name (ARN) of the listener.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the listener was created, specified in ISO-8601
    -- format.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The ID of the listener.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the listener was last updated, specified in
    -- ISO-8601 format.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The name of the listener.
    name :: Prelude.Maybe Prelude.Text,
    -- | The listener port.
    port :: Prelude.Maybe Prelude.Natural,
    -- | The listener protocol.
    protocol :: Prelude.Maybe ListenerProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListenerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listenerSummary_arn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'createdAt', 'listenerSummary_createdAt' - The date and time that the listener was created, specified in ISO-8601
-- format.
--
-- 'id', 'listenerSummary_id' - The ID of the listener.
--
-- 'lastUpdatedAt', 'listenerSummary_lastUpdatedAt' - The date and time that the listener was last updated, specified in
-- ISO-8601 format.
--
-- 'name', 'listenerSummary_name' - The name of the listener.
--
-- 'port', 'listenerSummary_port' - The listener port.
--
-- 'protocol', 'listenerSummary_protocol' - The listener protocol.
newListenerSummary ::
  ListenerSummary
newListenerSummary =
  ListenerSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      port = Prelude.Nothing,
      protocol = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the listener.
listenerSummary_arn :: Lens.Lens' ListenerSummary (Prelude.Maybe Prelude.Text)
listenerSummary_arn = Lens.lens (\ListenerSummary' {arn} -> arn) (\s@ListenerSummary' {} a -> s {arn = a} :: ListenerSummary)

-- | The date and time that the listener was created, specified in ISO-8601
-- format.
listenerSummary_createdAt :: Lens.Lens' ListenerSummary (Prelude.Maybe Prelude.UTCTime)
listenerSummary_createdAt = Lens.lens (\ListenerSummary' {createdAt} -> createdAt) (\s@ListenerSummary' {} a -> s {createdAt = a} :: ListenerSummary) Prelude.. Lens.mapping Data._Time

-- | The ID of the listener.
listenerSummary_id :: Lens.Lens' ListenerSummary (Prelude.Maybe Prelude.Text)
listenerSummary_id = Lens.lens (\ListenerSummary' {id} -> id) (\s@ListenerSummary' {} a -> s {id = a} :: ListenerSummary)

-- | The date and time that the listener was last updated, specified in
-- ISO-8601 format.
listenerSummary_lastUpdatedAt :: Lens.Lens' ListenerSummary (Prelude.Maybe Prelude.UTCTime)
listenerSummary_lastUpdatedAt = Lens.lens (\ListenerSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@ListenerSummary' {} a -> s {lastUpdatedAt = a} :: ListenerSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the listener.
listenerSummary_name :: Lens.Lens' ListenerSummary (Prelude.Maybe Prelude.Text)
listenerSummary_name = Lens.lens (\ListenerSummary' {name} -> name) (\s@ListenerSummary' {} a -> s {name = a} :: ListenerSummary)

-- | The listener port.
listenerSummary_port :: Lens.Lens' ListenerSummary (Prelude.Maybe Prelude.Natural)
listenerSummary_port = Lens.lens (\ListenerSummary' {port} -> port) (\s@ListenerSummary' {} a -> s {port = a} :: ListenerSummary)

-- | The listener protocol.
listenerSummary_protocol :: Lens.Lens' ListenerSummary (Prelude.Maybe ListenerProtocol)
listenerSummary_protocol = Lens.lens (\ListenerSummary' {protocol} -> protocol) (\s@ListenerSummary' {} a -> s {protocol = a} :: ListenerSummary)

instance Data.FromJSON ListenerSummary where
  parseJSON =
    Data.withObject
      "ListenerSummary"
      ( \x ->
          ListenerSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "port")
            Prelude.<*> (x Data..:? "protocol")
      )

instance Prelude.Hashable ListenerSummary where
  hashWithSalt _salt ListenerSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` protocol

instance Prelude.NFData ListenerSummary where
  rnf ListenerSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf protocol
