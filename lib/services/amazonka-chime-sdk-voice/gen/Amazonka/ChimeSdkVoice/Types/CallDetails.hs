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
-- Module      : Amazonka.ChimeSdkVoice.Types.CallDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.CallDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of an Amazon Chime SDK Voice Connector call.
--
-- /See:/ 'newCallDetails' smart constructor.
data CallDetails = CallDetails'
  { -- | Identifies a person as the caller or the callee.
    isCaller :: Prelude.Maybe Prelude.Bool,
    -- | The transaction ID of a Voice Connector call.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The Voice Connector ID.
    voiceConnectorId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CallDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isCaller', 'callDetails_isCaller' - Identifies a person as the caller or the callee.
--
-- 'transactionId', 'callDetails_transactionId' - The transaction ID of a Voice Connector call.
--
-- 'voiceConnectorId', 'callDetails_voiceConnectorId' - The Voice Connector ID.
newCallDetails ::
  CallDetails
newCallDetails =
  CallDetails'
    { isCaller = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      voiceConnectorId = Prelude.Nothing
    }

-- | Identifies a person as the caller or the callee.
callDetails_isCaller :: Lens.Lens' CallDetails (Prelude.Maybe Prelude.Bool)
callDetails_isCaller = Lens.lens (\CallDetails' {isCaller} -> isCaller) (\s@CallDetails' {} a -> s {isCaller = a} :: CallDetails)

-- | The transaction ID of a Voice Connector call.
callDetails_transactionId :: Lens.Lens' CallDetails (Prelude.Maybe Prelude.Text)
callDetails_transactionId = Lens.lens (\CallDetails' {transactionId} -> transactionId) (\s@CallDetails' {} a -> s {transactionId = a} :: CallDetails)

-- | The Voice Connector ID.
callDetails_voiceConnectorId :: Lens.Lens' CallDetails (Prelude.Maybe Prelude.Text)
callDetails_voiceConnectorId = Lens.lens (\CallDetails' {voiceConnectorId} -> voiceConnectorId) (\s@CallDetails' {} a -> s {voiceConnectorId = a} :: CallDetails)

instance Data.FromJSON CallDetails where
  parseJSON =
    Data.withObject
      "CallDetails"
      ( \x ->
          CallDetails'
            Prelude.<$> (x Data..:? "IsCaller")
            Prelude.<*> (x Data..:? "TransactionId")
            Prelude.<*> (x Data..:? "VoiceConnectorId")
      )

instance Prelude.Hashable CallDetails where
  hashWithSalt _salt CallDetails' {..} =
    _salt
      `Prelude.hashWithSalt` isCaller
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData CallDetails where
  rnf CallDetails' {..} =
    Prelude.rnf isCaller
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf voiceConnectorId
