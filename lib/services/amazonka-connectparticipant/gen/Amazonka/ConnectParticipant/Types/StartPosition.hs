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
-- Module      : Amazonka.ConnectParticipant.Types.StartPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.StartPosition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filtering option for where to start. For example, if you sent 100
-- messages, start with message 50.
--
-- /See:/ 'newStartPosition' smart constructor.
data StartPosition = StartPosition'
  { -- | The time in ISO format where to start.
    --
    -- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
    -- example, 2019-11-08T02:41:28.172Z.
    absoluteTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the message or event where to start.
    id :: Prelude.Maybe Prelude.Text,
    -- | The start position of the most recent message where you want to start.
    mostRecent :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartPosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absoluteTime', 'startPosition_absoluteTime' - The time in ISO format where to start.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
--
-- 'id', 'startPosition_id' - The ID of the message or event where to start.
--
-- 'mostRecent', 'startPosition_mostRecent' - The start position of the most recent message where you want to start.
newStartPosition ::
  StartPosition
newStartPosition =
  StartPosition'
    { absoluteTime = Prelude.Nothing,
      id = Prelude.Nothing,
      mostRecent = Prelude.Nothing
    }

-- | The time in ISO format where to start.
--
-- It\'s specified in ISO 8601 format: yyyy-MM-ddThh:mm:ss.SSSZ. For
-- example, 2019-11-08T02:41:28.172Z.
startPosition_absoluteTime :: Lens.Lens' StartPosition (Prelude.Maybe Prelude.Text)
startPosition_absoluteTime = Lens.lens (\StartPosition' {absoluteTime} -> absoluteTime) (\s@StartPosition' {} a -> s {absoluteTime = a} :: StartPosition)

-- | The ID of the message or event where to start.
startPosition_id :: Lens.Lens' StartPosition (Prelude.Maybe Prelude.Text)
startPosition_id = Lens.lens (\StartPosition' {id} -> id) (\s@StartPosition' {} a -> s {id = a} :: StartPosition)

-- | The start position of the most recent message where you want to start.
startPosition_mostRecent :: Lens.Lens' StartPosition (Prelude.Maybe Prelude.Natural)
startPosition_mostRecent = Lens.lens (\StartPosition' {mostRecent} -> mostRecent) (\s@StartPosition' {} a -> s {mostRecent = a} :: StartPosition)

instance Prelude.Hashable StartPosition where
  hashWithSalt _salt StartPosition' {..} =
    _salt
      `Prelude.hashWithSalt` absoluteTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` mostRecent

instance Prelude.NFData StartPosition where
  rnf StartPosition' {..} =
    Prelude.rnf absoluteTime `Prelude.seq`
      Prelude.rnf id `Prelude.seq`
        Prelude.rnf mostRecent

instance Data.ToJSON StartPosition where
  toJSON StartPosition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AbsoluteTime" Data..=) Prelude.<$> absoluteTime,
            ("Id" Data..=) Prelude.<$> id,
            ("MostRecent" Data..=) Prelude.<$> mostRecent
          ]
      )
