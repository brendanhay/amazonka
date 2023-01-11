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
-- Module      : Amazonka.MwAA.Types.LastUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MwAA.Types.LastUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types.UpdateError
import Amazonka.MwAA.Types.UpdateStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes the status of the last update on the environment, and any
-- errors that were encountered.
--
-- /See:/ 'newLastUpdate' smart constructor.
data LastUpdate = LastUpdate'
  { -- | The day and time of the last update on the environment.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The error that was encountered during the last update of the
    -- environment.
    error :: Prelude.Maybe UpdateError,
    -- | The source of the last update to the environment. Includes internal
    -- processes by Amazon MWAA, such as an environment maintenance update.
    source :: Prelude.Maybe Prelude.Text,
    -- | The status of the last update on the environment.
    status :: Prelude.Maybe UpdateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'lastUpdate_createdAt' - The day and time of the last update on the environment.
--
-- 'error', 'lastUpdate_error' - The error that was encountered during the last update of the
-- environment.
--
-- 'source', 'lastUpdate_source' - The source of the last update to the environment. Includes internal
-- processes by Amazon MWAA, such as an environment maintenance update.
--
-- 'status', 'lastUpdate_status' - The status of the last update on the environment.
newLastUpdate ::
  LastUpdate
newLastUpdate =
  LastUpdate'
    { createdAt = Prelude.Nothing,
      error = Prelude.Nothing,
      source = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The day and time of the last update on the environment.
lastUpdate_createdAt :: Lens.Lens' LastUpdate (Prelude.Maybe Prelude.UTCTime)
lastUpdate_createdAt = Lens.lens (\LastUpdate' {createdAt} -> createdAt) (\s@LastUpdate' {} a -> s {createdAt = a} :: LastUpdate) Prelude.. Lens.mapping Data._Time

-- | The error that was encountered during the last update of the
-- environment.
lastUpdate_error :: Lens.Lens' LastUpdate (Prelude.Maybe UpdateError)
lastUpdate_error = Lens.lens (\LastUpdate' {error} -> error) (\s@LastUpdate' {} a -> s {error = a} :: LastUpdate)

-- | The source of the last update to the environment. Includes internal
-- processes by Amazon MWAA, such as an environment maintenance update.
lastUpdate_source :: Lens.Lens' LastUpdate (Prelude.Maybe Prelude.Text)
lastUpdate_source = Lens.lens (\LastUpdate' {source} -> source) (\s@LastUpdate' {} a -> s {source = a} :: LastUpdate)

-- | The status of the last update on the environment.
lastUpdate_status :: Lens.Lens' LastUpdate (Prelude.Maybe UpdateStatus)
lastUpdate_status = Lens.lens (\LastUpdate' {status} -> status) (\s@LastUpdate' {} a -> s {status = a} :: LastUpdate)

instance Data.FromJSON LastUpdate where
  parseJSON =
    Data.withObject
      "LastUpdate"
      ( \x ->
          LastUpdate'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable LastUpdate where
  hashWithSalt _salt LastUpdate' {..} =
    _salt `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` status

instance Prelude.NFData LastUpdate where
  rnf LastUpdate' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf error
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf status
