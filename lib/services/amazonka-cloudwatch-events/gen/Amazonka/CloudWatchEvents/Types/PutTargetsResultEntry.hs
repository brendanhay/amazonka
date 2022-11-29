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
-- Module      : Amazonka.CloudWatchEvents.Types.PutTargetsResultEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.PutTargetsResultEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a target that failed to be added to a rule.
--
-- /See:/ 'newPutTargetsResultEntry' smart constructor.
data PutTargetsResultEntry = PutTargetsResultEntry'
  { -- | The ID of the target.
    targetId :: Prelude.Maybe Prelude.Text,
    -- | The error message that explains why the target addition failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The error code that indicates why the target addition failed. If the
    -- value is @ConcurrentModificationException@, too many requests were made
    -- at the same time.
    errorCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutTargetsResultEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetId', 'putTargetsResultEntry_targetId' - The ID of the target.
--
-- 'errorMessage', 'putTargetsResultEntry_errorMessage' - The error message that explains why the target addition failed.
--
-- 'errorCode', 'putTargetsResultEntry_errorCode' - The error code that indicates why the target addition failed. If the
-- value is @ConcurrentModificationException@, too many requests were made
-- at the same time.
newPutTargetsResultEntry ::
  PutTargetsResultEntry
newPutTargetsResultEntry =
  PutTargetsResultEntry'
    { targetId = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      errorCode = Prelude.Nothing
    }

-- | The ID of the target.
putTargetsResultEntry_targetId :: Lens.Lens' PutTargetsResultEntry (Prelude.Maybe Prelude.Text)
putTargetsResultEntry_targetId = Lens.lens (\PutTargetsResultEntry' {targetId} -> targetId) (\s@PutTargetsResultEntry' {} a -> s {targetId = a} :: PutTargetsResultEntry)

-- | The error message that explains why the target addition failed.
putTargetsResultEntry_errorMessage :: Lens.Lens' PutTargetsResultEntry (Prelude.Maybe Prelude.Text)
putTargetsResultEntry_errorMessage = Lens.lens (\PutTargetsResultEntry' {errorMessage} -> errorMessage) (\s@PutTargetsResultEntry' {} a -> s {errorMessage = a} :: PutTargetsResultEntry)

-- | The error code that indicates why the target addition failed. If the
-- value is @ConcurrentModificationException@, too many requests were made
-- at the same time.
putTargetsResultEntry_errorCode :: Lens.Lens' PutTargetsResultEntry (Prelude.Maybe Prelude.Text)
putTargetsResultEntry_errorCode = Lens.lens (\PutTargetsResultEntry' {errorCode} -> errorCode) (\s@PutTargetsResultEntry' {} a -> s {errorCode = a} :: PutTargetsResultEntry)

instance Core.FromJSON PutTargetsResultEntry where
  parseJSON =
    Core.withObject
      "PutTargetsResultEntry"
      ( \x ->
          PutTargetsResultEntry'
            Prelude.<$> (x Core..:? "TargetId")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "ErrorCode")
      )

instance Prelude.Hashable PutTargetsResultEntry where
  hashWithSalt _salt PutTargetsResultEntry' {..} =
    _salt `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode

instance Prelude.NFData PutTargetsResultEntry where
  rnf PutTargetsResultEntry' {..} =
    Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
