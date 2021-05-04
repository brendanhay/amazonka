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
-- Module      : Network.AWS.CloudSearchDomains.Types.SuggestStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.SuggestStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the resource id (@rid@) and the time it took to process the
-- request (@timems@).
--
-- /See:/ 'newSuggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { -- | How long it took to process the request, in milliseconds.
    timems :: Prelude.Maybe Prelude.Integer,
    -- | The encrypted resource ID for the request.
    rid :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SuggestStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timems', 'suggestStatus_timems' - How long it took to process the request, in milliseconds.
--
-- 'rid', 'suggestStatus_rid' - The encrypted resource ID for the request.
newSuggestStatus ::
  SuggestStatus
newSuggestStatus =
  SuggestStatus'
    { timems = Prelude.Nothing,
      rid = Prelude.Nothing
    }

-- | How long it took to process the request, in milliseconds.
suggestStatus_timems :: Lens.Lens' SuggestStatus (Prelude.Maybe Prelude.Integer)
suggestStatus_timems = Lens.lens (\SuggestStatus' {timems} -> timems) (\s@SuggestStatus' {} a -> s {timems = a} :: SuggestStatus)

-- | The encrypted resource ID for the request.
suggestStatus_rid :: Lens.Lens' SuggestStatus (Prelude.Maybe Prelude.Text)
suggestStatus_rid = Lens.lens (\SuggestStatus' {rid} -> rid) (\s@SuggestStatus' {} a -> s {rid = a} :: SuggestStatus)

instance Prelude.FromJSON SuggestStatus where
  parseJSON =
    Prelude.withObject
      "SuggestStatus"
      ( \x ->
          SuggestStatus'
            Prelude.<$> (x Prelude..:? "timems")
            Prelude.<*> (x Prelude..:? "rid")
      )

instance Prelude.Hashable SuggestStatus

instance Prelude.NFData SuggestStatus
