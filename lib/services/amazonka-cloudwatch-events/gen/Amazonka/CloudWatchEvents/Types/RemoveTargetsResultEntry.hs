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
-- Module      : Amazonka.CloudWatchEvents.Types.RemoveTargetsResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.RemoveTargetsResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a target that failed to be removed from a rule.
--
-- /See:/ 'newRemoveTargetsResultEntry' smart constructor.
data RemoveTargetsResultEntry = RemoveTargetsResultEntry'
  { -- | The ID of the target.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The error code that indicates why the target removal failed. If the
    -- value is @ConcurrentModificationException@, too many requests were made
    -- at the same time.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The error message that explains why the target removal failed.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'errorCode', 'removeTargetsResultEntry_errorCode' - The error code that indicates why the target removal failed. If the
-- value is @ConcurrentModificationException@, too many requests were made
-- at the same time.
--
-- 'errorMessage', 'removeTargetsResultEntry_errorMessage' - The error message that explains why the target removal failed.
newRemoveTargetsResultEntry ::
  RemoveTargetsResultEntry
newRemoveTargetsResultEntry =
  RemoveTargetsResultEntry'
    { targetId =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The ID of the target.
removeTargetsResultEntry_targetId :: Lens.Lens' RemoveTargetsResultEntry (Prelude.Maybe Prelude.Text)
removeTargetsResultEntry_targetId = Lens.lens (\RemoveTargetsResultEntry' {targetId} -> targetId) (\s@RemoveTargetsResultEntry' {} a -> s {targetId = a} :: RemoveTargetsResultEntry)

-- | The error code that indicates why the target removal failed. If the
-- value is @ConcurrentModificationException@, too many requests were made
-- at the same time.
removeTargetsResultEntry_errorCode :: Lens.Lens' RemoveTargetsResultEntry (Prelude.Maybe Prelude.Text)
removeTargetsResultEntry_errorCode = Lens.lens (\RemoveTargetsResultEntry' {errorCode} -> errorCode) (\s@RemoveTargetsResultEntry' {} a -> s {errorCode = a} :: RemoveTargetsResultEntry)

-- | The error message that explains why the target removal failed.
removeTargetsResultEntry_errorMessage :: Lens.Lens' RemoveTargetsResultEntry (Prelude.Maybe Prelude.Text)
removeTargetsResultEntry_errorMessage = Lens.lens (\RemoveTargetsResultEntry' {errorMessage} -> errorMessage) (\s@RemoveTargetsResultEntry' {} a -> s {errorMessage = a} :: RemoveTargetsResultEntry)

instance Core.FromJSON RemoveTargetsResultEntry where
  parseJSON =
    Core.withObject
      "RemoveTargetsResultEntry"
      ( \x ->
          RemoveTargetsResultEntry'
            Prelude.<$> (x Core..:? "TargetId")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable RemoveTargetsResultEntry where
  hashWithSalt _salt RemoveTargetsResultEntry' {..} =
    _salt `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData RemoveTargetsResultEntry where
  rnf RemoveTargetsResultEntry' {..} =
    Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
