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
-- Module      : Amazonka.CloudTrailData.Types.ResultErrorEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrailData.Types.ResultErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Includes the error code and error message for events that could not be
-- ingested by CloudTrail.
--
-- /See:/ 'newResultErrorEntry' smart constructor.
data ResultErrorEntry = ResultErrorEntry'
  { -- | The error code for events that could not be ingested by CloudTrail.
    -- Possible error codes include: @FieldTooLong@, @FieldNotFound@,
    -- @InvalidChecksum@, @InvalidData@, @InvalidRecipient@,
    -- @InvalidEventSource@, @AccountNotSubscribed@, @Throttling@, and
    -- @InternalFailure@.
    errorCode :: Prelude.Text,
    -- | The message that describes the error for events that could not be
    -- ingested by CloudTrail.
    errorMessage :: Prelude.Text,
    -- | The original event ID from the source event that could not be ingested
    -- by CloudTrail.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResultErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'resultErrorEntry_errorCode' - The error code for events that could not be ingested by CloudTrail.
-- Possible error codes include: @FieldTooLong@, @FieldNotFound@,
-- @InvalidChecksum@, @InvalidData@, @InvalidRecipient@,
-- @InvalidEventSource@, @AccountNotSubscribed@, @Throttling@, and
-- @InternalFailure@.
--
-- 'errorMessage', 'resultErrorEntry_errorMessage' - The message that describes the error for events that could not be
-- ingested by CloudTrail.
--
-- 'id', 'resultErrorEntry_id' - The original event ID from the source event that could not be ingested
-- by CloudTrail.
newResultErrorEntry ::
  -- | 'errorCode'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  ResultErrorEntry
newResultErrorEntry pErrorCode_ pErrorMessage_ pId_ =
  ResultErrorEntry'
    { errorCode = pErrorCode_,
      errorMessage = pErrorMessage_,
      id = pId_
    }

-- | The error code for events that could not be ingested by CloudTrail.
-- Possible error codes include: @FieldTooLong@, @FieldNotFound@,
-- @InvalidChecksum@, @InvalidData@, @InvalidRecipient@,
-- @InvalidEventSource@, @AccountNotSubscribed@, @Throttling@, and
-- @InternalFailure@.
resultErrorEntry_errorCode :: Lens.Lens' ResultErrorEntry Prelude.Text
resultErrorEntry_errorCode = Lens.lens (\ResultErrorEntry' {errorCode} -> errorCode) (\s@ResultErrorEntry' {} a -> s {errorCode = a} :: ResultErrorEntry)

-- | The message that describes the error for events that could not be
-- ingested by CloudTrail.
resultErrorEntry_errorMessage :: Lens.Lens' ResultErrorEntry Prelude.Text
resultErrorEntry_errorMessage = Lens.lens (\ResultErrorEntry' {errorMessage} -> errorMessage) (\s@ResultErrorEntry' {} a -> s {errorMessage = a} :: ResultErrorEntry)

-- | The original event ID from the source event that could not be ingested
-- by CloudTrail.
resultErrorEntry_id :: Lens.Lens' ResultErrorEntry Prelude.Text
resultErrorEntry_id = Lens.lens (\ResultErrorEntry' {id} -> id) (\s@ResultErrorEntry' {} a -> s {id = a} :: ResultErrorEntry)

instance Data.FromJSON ResultErrorEntry where
  parseJSON =
    Data.withObject
      "ResultErrorEntry"
      ( \x ->
          ResultErrorEntry'
            Prelude.<$> (x Data..: "errorCode")
            Prelude.<*> (x Data..: "errorMessage")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable ResultErrorEntry where
  hashWithSalt _salt ResultErrorEntry' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` id

instance Prelude.NFData ResultErrorEntry where
  rnf ResultErrorEntry' {..} =
    Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf id
