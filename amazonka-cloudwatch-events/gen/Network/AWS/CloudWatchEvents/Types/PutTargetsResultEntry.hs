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
-- Module      : Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutTargetsResultEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON PutTargetsResultEntry where
  parseJSON =
    Prelude.withObject
      "PutTargetsResultEntry"
      ( \x ->
          PutTargetsResultEntry'
            Prelude.<$> (x Prelude..:? "TargetId")
            Prelude.<*> (x Prelude..:? "ErrorMessage")
            Prelude.<*> (x Prelude..:? "ErrorCode")
      )

instance Prelude.Hashable PutTargetsResultEntry

instance Prelude.NFData PutTargetsResultEntry
