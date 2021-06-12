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
-- Module      : Network.AWS.XRay.Types.UnprocessedTraceSegment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.UnprocessedTraceSegment where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a segment that failed processing.
--
-- /See:/ 'newUnprocessedTraceSegment' smart constructor.
data UnprocessedTraceSegment = UnprocessedTraceSegment'
  { -- | The error message.
    message :: Core.Maybe Core.Text,
    -- | The segment\'s ID.
    id :: Core.Maybe Core.Text,
    -- | The error that caused processing to fail.
    errorCode :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnprocessedTraceSegment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'unprocessedTraceSegment_message' - The error message.
--
-- 'id', 'unprocessedTraceSegment_id' - The segment\'s ID.
--
-- 'errorCode', 'unprocessedTraceSegment_errorCode' - The error that caused processing to fail.
newUnprocessedTraceSegment ::
  UnprocessedTraceSegment
newUnprocessedTraceSegment =
  UnprocessedTraceSegment'
    { message = Core.Nothing,
      id = Core.Nothing,
      errorCode = Core.Nothing
    }

-- | The error message.
unprocessedTraceSegment_message :: Lens.Lens' UnprocessedTraceSegment (Core.Maybe Core.Text)
unprocessedTraceSegment_message = Lens.lens (\UnprocessedTraceSegment' {message} -> message) (\s@UnprocessedTraceSegment' {} a -> s {message = a} :: UnprocessedTraceSegment)

-- | The segment\'s ID.
unprocessedTraceSegment_id :: Lens.Lens' UnprocessedTraceSegment (Core.Maybe Core.Text)
unprocessedTraceSegment_id = Lens.lens (\UnprocessedTraceSegment' {id} -> id) (\s@UnprocessedTraceSegment' {} a -> s {id = a} :: UnprocessedTraceSegment)

-- | The error that caused processing to fail.
unprocessedTraceSegment_errorCode :: Lens.Lens' UnprocessedTraceSegment (Core.Maybe Core.Text)
unprocessedTraceSegment_errorCode = Lens.lens (\UnprocessedTraceSegment' {errorCode} -> errorCode) (\s@UnprocessedTraceSegment' {} a -> s {errorCode = a} :: UnprocessedTraceSegment)

instance Core.FromJSON UnprocessedTraceSegment where
  parseJSON =
    Core.withObject
      "UnprocessedTraceSegment"
      ( \x ->
          UnprocessedTraceSegment'
            Core.<$> (x Core..:? "Message")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "ErrorCode")
      )

instance Core.Hashable UnprocessedTraceSegment

instance Core.NFData UnprocessedTraceSegment
