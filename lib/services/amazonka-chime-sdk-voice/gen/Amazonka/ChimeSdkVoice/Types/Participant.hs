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
-- Module      : Amazonka.ChimeSdkVoice.Types.Participant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkVoice.Types.Participant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newParticipant' smart constructor.
data Participant = Participant'
  { phoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    proxyPhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Participant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumber', 'participant_phoneNumber' - Undocumented member.
--
-- 'proxyPhoneNumber', 'participant_proxyPhoneNumber' - Undocumented member.
newParticipant ::
  Participant
newParticipant =
  Participant'
    { phoneNumber = Prelude.Nothing,
      proxyPhoneNumber = Prelude.Nothing
    }

-- | Undocumented member.
participant_phoneNumber :: Lens.Lens' Participant (Prelude.Maybe Prelude.Text)
participant_phoneNumber = Lens.lens (\Participant' {phoneNumber} -> phoneNumber) (\s@Participant' {} a -> s {phoneNumber = a} :: Participant) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
participant_proxyPhoneNumber :: Lens.Lens' Participant (Prelude.Maybe Prelude.Text)
participant_proxyPhoneNumber = Lens.lens (\Participant' {proxyPhoneNumber} -> proxyPhoneNumber) (\s@Participant' {} a -> s {proxyPhoneNumber = a} :: Participant) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON Participant where
  parseJSON =
    Data.withObject
      "Participant"
      ( \x ->
          Participant'
            Prelude.<$> (x Data..:? "PhoneNumber")
            Prelude.<*> (x Data..:? "ProxyPhoneNumber")
      )

instance Prelude.Hashable Participant where
  hashWithSalt _salt Participant' {..} =
    _salt
      `Prelude.hashWithSalt` phoneNumber
      `Prelude.hashWithSalt` proxyPhoneNumber

instance Prelude.NFData Participant where
  rnf Participant' {..} =
    Prelude.rnf phoneNumber `Prelude.seq`
      Prelude.rnf proxyPhoneNumber
