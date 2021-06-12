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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the resource id (@rid@) and the time it took to process the
-- request (@timems@).
--
-- /See:/ 'newSuggestStatus' smart constructor.
data SuggestStatus = SuggestStatus'
  { -- | How long it took to process the request, in milliseconds.
    timems :: Core.Maybe Core.Integer,
    -- | The encrypted resource ID for the request.
    rid :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { timems = Core.Nothing,
      rid = Core.Nothing
    }

-- | How long it took to process the request, in milliseconds.
suggestStatus_timems :: Lens.Lens' SuggestStatus (Core.Maybe Core.Integer)
suggestStatus_timems = Lens.lens (\SuggestStatus' {timems} -> timems) (\s@SuggestStatus' {} a -> s {timems = a} :: SuggestStatus)

-- | The encrypted resource ID for the request.
suggestStatus_rid :: Lens.Lens' SuggestStatus (Core.Maybe Core.Text)
suggestStatus_rid = Lens.lens (\SuggestStatus' {rid} -> rid) (\s@SuggestStatus' {} a -> s {rid = a} :: SuggestStatus)

instance Core.FromJSON SuggestStatus where
  parseJSON =
    Core.withObject
      "SuggestStatus"
      ( \x ->
          SuggestStatus'
            Core.<$> (x Core..:? "timems") Core.<*> (x Core..:? "rid")
      )

instance Core.Hashable SuggestStatus

instance Core.NFData SuggestStatus
