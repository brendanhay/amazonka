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
-- Module      : Amazonka.Lightsail.Types.Session
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Session where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a web-based, remote graphical user interface (GUI), NICE DCV
-- session. The session is used to access a virtual computer’s operating
-- system or application.
--
-- /See:/ 'newSession' smart constructor.
data Session = Session'
  { -- | When true, this Boolean value indicates the primary session for the
    -- specified resource.
    isPrimary :: Prelude.Maybe Prelude.Bool,
    -- | The session name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The session URL.
    url :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Session' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPrimary', 'session_isPrimary' - When true, this Boolean value indicates the primary session for the
-- specified resource.
--
-- 'name', 'session_name' - The session name.
--
-- 'url', 'session_url' - The session URL.
newSession ::
  Session
newSession =
  Session'
    { isPrimary = Prelude.Nothing,
      name = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | When true, this Boolean value indicates the primary session for the
-- specified resource.
session_isPrimary :: Lens.Lens' Session (Prelude.Maybe Prelude.Bool)
session_isPrimary = Lens.lens (\Session' {isPrimary} -> isPrimary) (\s@Session' {} a -> s {isPrimary = a} :: Session)

-- | The session name.
session_name :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_name = Lens.lens (\Session' {name} -> name) (\s@Session' {} a -> s {name = a} :: Session)

-- | The session URL.
session_url :: Lens.Lens' Session (Prelude.Maybe Prelude.Text)
session_url = Lens.lens (\Session' {url} -> url) (\s@Session' {} a -> s {url = a} :: Session) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Session where
  parseJSON =
    Data.withObject
      "Session"
      ( \x ->
          Session'
            Prelude.<$> (x Data..:? "isPrimary")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable Session where
  hashWithSalt _salt Session' {..} =
    _salt
      `Prelude.hashWithSalt` isPrimary
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` url

instance Prelude.NFData Session where
  rnf Session' {..} =
    Prelude.rnf isPrimary
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf url
