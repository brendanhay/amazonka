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
-- Module      : Amazonka.XRay.Types.UnprocessedTraceSegment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.UnprocessedTraceSegment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a segment that failed processing.
--
-- /See:/ 'newUnprocessedTraceSegment' smart constructor.
data UnprocessedTraceSegment = UnprocessedTraceSegment'
  { -- | The error that caused processing to fail.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The segment\'s ID.
    id :: Prelude.Maybe Prelude.Text,
    -- | The error message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnprocessedTraceSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'unprocessedTraceSegment_errorCode' - The error that caused processing to fail.
--
-- 'id', 'unprocessedTraceSegment_id' - The segment\'s ID.
--
-- 'message', 'unprocessedTraceSegment_message' - The error message.
newUnprocessedTraceSegment ::
  UnprocessedTraceSegment
newUnprocessedTraceSegment =
  UnprocessedTraceSegment'
    { errorCode =
        Prelude.Nothing,
      id = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The error that caused processing to fail.
unprocessedTraceSegment_errorCode :: Lens.Lens' UnprocessedTraceSegment (Prelude.Maybe Prelude.Text)
unprocessedTraceSegment_errorCode = Lens.lens (\UnprocessedTraceSegment' {errorCode} -> errorCode) (\s@UnprocessedTraceSegment' {} a -> s {errorCode = a} :: UnprocessedTraceSegment)

-- | The segment\'s ID.
unprocessedTraceSegment_id :: Lens.Lens' UnprocessedTraceSegment (Prelude.Maybe Prelude.Text)
unprocessedTraceSegment_id = Lens.lens (\UnprocessedTraceSegment' {id} -> id) (\s@UnprocessedTraceSegment' {} a -> s {id = a} :: UnprocessedTraceSegment)

-- | The error message.
unprocessedTraceSegment_message :: Lens.Lens' UnprocessedTraceSegment (Prelude.Maybe Prelude.Text)
unprocessedTraceSegment_message = Lens.lens (\UnprocessedTraceSegment' {message} -> message) (\s@UnprocessedTraceSegment' {} a -> s {message = a} :: UnprocessedTraceSegment)

instance Data.FromJSON UnprocessedTraceSegment where
  parseJSON =
    Data.withObject
      "UnprocessedTraceSegment"
      ( \x ->
          UnprocessedTraceSegment'
            Prelude.<$> (x Data..:? "ErrorCode")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable UnprocessedTraceSegment where
  hashWithSalt _salt UnprocessedTraceSegment' {..} =
    _salt `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` message

instance Prelude.NFData UnprocessedTraceSegment where
  rnf UnprocessedTraceSegment' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf message
