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
-- Module      : Amazonka.CloudSearchDomains.Types.SuggestStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.SuggestStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the resource id (@rid@) and the time it took to process the
-- request (@timems@).
--
-- /See:/ 'newSuggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { -- | The encrypted resource ID for the request.
    rid :: Prelude.Maybe Prelude.Text,
    -- | How long it took to process the request, in milliseconds.
    timems :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuggestStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rid', 'suggestStatus_rid' - The encrypted resource ID for the request.
--
-- 'timems', 'suggestStatus_timems' - How long it took to process the request, in milliseconds.
newSuggestStatus ::
  SuggestStatus
newSuggestStatus =
  SuggestStatus'
    { rid = Prelude.Nothing,
      timems = Prelude.Nothing
    }

-- | The encrypted resource ID for the request.
suggestStatus_rid :: Lens.Lens' SuggestStatus (Prelude.Maybe Prelude.Text)
suggestStatus_rid = Lens.lens (\SuggestStatus' {rid} -> rid) (\s@SuggestStatus' {} a -> s {rid = a} :: SuggestStatus)

-- | How long it took to process the request, in milliseconds.
suggestStatus_timems :: Lens.Lens' SuggestStatus (Prelude.Maybe Prelude.Integer)
suggestStatus_timems = Lens.lens (\SuggestStatus' {timems} -> timems) (\s@SuggestStatus' {} a -> s {timems = a} :: SuggestStatus)

instance Data.FromJSON SuggestStatus where
  parseJSON =
    Data.withObject
      "SuggestStatus"
      ( \x ->
          SuggestStatus'
            Prelude.<$> (x Data..:? "rid") Prelude.<*> (x Data..:? "timems")
      )

instance Prelude.Hashable SuggestStatus where
  hashWithSalt _salt SuggestStatus' {..} =
    _salt `Prelude.hashWithSalt` rid
      `Prelude.hashWithSalt` timems

instance Prelude.NFData SuggestStatus where
  rnf SuggestStatus' {..} =
    Prelude.rnf rid `Prelude.seq` Prelude.rnf timems
