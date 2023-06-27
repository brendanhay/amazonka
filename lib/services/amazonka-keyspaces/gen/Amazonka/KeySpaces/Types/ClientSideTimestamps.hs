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
-- Module      : Amazonka.KeySpaces.Types.ClientSideTimestamps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KeySpaces.Types.ClientSideTimestamps where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KeySpaces.Types.ClientSideTimestampsStatus
import qualified Amazonka.Prelude as Prelude

-- | The client-side timestamp setting of the table.
--
-- For more information, see
-- <https://docs.aws.amazon.com/keyspaces/latest/devguide/client-side-timestamps-how-it-works.html How it works: Amazon Keyspaces client-side timestamps>
-- in the /Amazon Keyspaces Developer Guide/.
--
-- /See:/ 'newClientSideTimestamps' smart constructor.
data ClientSideTimestamps = ClientSideTimestamps'
  { -- | Shows how to enable client-side timestamps settings for the specified
    -- table.
    status :: ClientSideTimestampsStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientSideTimestamps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'clientSideTimestamps_status' - Shows how to enable client-side timestamps settings for the specified
-- table.
newClientSideTimestamps ::
  -- | 'status'
  ClientSideTimestampsStatus ->
  ClientSideTimestamps
newClientSideTimestamps pStatus_ =
  ClientSideTimestamps' {status = pStatus_}

-- | Shows how to enable client-side timestamps settings for the specified
-- table.
clientSideTimestamps_status :: Lens.Lens' ClientSideTimestamps ClientSideTimestampsStatus
clientSideTimestamps_status = Lens.lens (\ClientSideTimestamps' {status} -> status) (\s@ClientSideTimestamps' {} a -> s {status = a} :: ClientSideTimestamps)

instance Data.FromJSON ClientSideTimestamps where
  parseJSON =
    Data.withObject
      "ClientSideTimestamps"
      ( \x ->
          ClientSideTimestamps'
            Prelude.<$> (x Data..: "status")
      )

instance Prelude.Hashable ClientSideTimestamps where
  hashWithSalt _salt ClientSideTimestamps' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData ClientSideTimestamps where
  rnf ClientSideTimestamps' {..} = Prelude.rnf status

instance Data.ToJSON ClientSideTimestamps where
  toJSON ClientSideTimestamps' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )
