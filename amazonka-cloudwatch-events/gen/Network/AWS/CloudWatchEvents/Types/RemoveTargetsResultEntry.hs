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
-- Module      : Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RemoveTargetsResultEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a target that failed to be removed from a rule.
--
-- /See:/ 'newRemoveTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
  { -- | The ID of the target.
    targetId :: Core.Maybe Core.Text,
    -- | The error message that explains why the target removal failed.
    errorMessage :: Core.Maybe Core.Text,
    -- | The error code that indicates why the target removal failed. If the
    -- value is @ConcurrentModificationException@, too many requests were made
    -- at the same time.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RemoveTargetsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'removeTargetsResultEntry_targetId' - The ID of the target.
--
-- 'errorMessage', 'removeTargetsResultEntry_errorMessage' - The error message that explains why the target removal failed.
--
-- 'errorCode', 'removeTargetsResultEntry_errorCode' - The error code that indicates why the target removal failed. If the
-- value is @ConcurrentModificationException@, too many requests were made
-- at the same time.
newRemoveTargetsResultEntry ::
  RemoveTargetsResultEntry
newRemoveTargetsResultEntry =
  RemoveTargetsResultEntry'
    { targetId = Core.Nothing,
      errorMessage = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The ID of the target.
removeTargetsResultEntry_targetId :: Lens.Lens' RemoveTargetsResultEntry (Core.Maybe Core.Text)
removeTargetsResultEntry_targetId = Lens.lens (\RemoveTargetsResultEntry' {targetId} -> targetId) (\s@RemoveTargetsResultEntry' {} a -> s {targetId = a} :: RemoveTargetsResultEntry)

-- | The error message that explains why the target removal failed.
removeTargetsResultEntry_errorMessage :: Lens.Lens' RemoveTargetsResultEntry (Core.Maybe Core.Text)
removeTargetsResultEntry_errorMessage = Lens.lens (\RemoveTargetsResultEntry' {errorMessage} -> errorMessage) (\s@RemoveTargetsResultEntry' {} a -> s {errorMessage = a} :: RemoveTargetsResultEntry)

-- | The error code that indicates why the target removal failed. If the
-- value is @ConcurrentModificationException@, too many requests were made
-- at the same time.
removeTargetsResultEntry_errorCode :: Lens.Lens' RemoveTargetsResultEntry (Core.Maybe Core.Text)
removeTargetsResultEntry_errorCode = Lens.lens (\RemoveTargetsResultEntry' {errorCode} -> errorCode) (\s@RemoveTargetsResultEntry' {} a -> s {errorCode = a} :: RemoveTargetsResultEntry)

instance Core.FromJSON RemoveTargetsResultEntry where
  parseJSON =
    Core.withObject
      "RemoveTargetsResultEntry"
      ( \x ->
          RemoveTargetsResultEntry'
            Core.<$> (x Core..:? "TargetId")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable RemoveTargetsResultEntry

instance Core.NFData RemoveTargetsResultEntry
