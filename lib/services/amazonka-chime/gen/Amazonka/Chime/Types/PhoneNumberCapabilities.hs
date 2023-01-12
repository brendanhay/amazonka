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
-- Module      : Amazonka.Chime.Types.PhoneNumberCapabilities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberCapabilities where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The phone number capabilities for Amazon Chime Business Calling phone
-- numbers, such as enabled inbound and outbound calling and text
-- messaging.
--
-- /See:/ 'newPhoneNumberCapabilities' smart constructor.
data PhoneNumberCapabilities = PhoneNumberCapabilities'
  { -- | Allows or denies inbound calling for the specified phone number.
    inboundCall :: Prelude.Maybe Prelude.Bool,
    -- | Allows or denies inbound MMS messaging for the specified phone number.
    inboundMMS :: Prelude.Maybe Prelude.Bool,
    -- | Allows or denies inbound SMS messaging for the specified phone number.
    inboundSMS :: Prelude.Maybe Prelude.Bool,
    -- | Allows or denies outbound calling for the specified phone number.
    outboundCall :: Prelude.Maybe Prelude.Bool,
    -- | Allows or denies outbound MMS messaging for the specified phone number.
    outboundMMS :: Prelude.Maybe Prelude.Bool,
    -- | Allows or denies outbound SMS messaging for the specified phone number.
    outboundSMS :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberCapabilities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inboundCall', 'phoneNumberCapabilities_inboundCall' - Allows or denies inbound calling for the specified phone number.
--
-- 'inboundMMS', 'phoneNumberCapabilities_inboundMMS' - Allows or denies inbound MMS messaging for the specified phone number.
--
-- 'inboundSMS', 'phoneNumberCapabilities_inboundSMS' - Allows or denies inbound SMS messaging for the specified phone number.
--
-- 'outboundCall', 'phoneNumberCapabilities_outboundCall' - Allows or denies outbound calling for the specified phone number.
--
-- 'outboundMMS', 'phoneNumberCapabilities_outboundMMS' - Allows or denies outbound MMS messaging for the specified phone number.
--
-- 'outboundSMS', 'phoneNumberCapabilities_outboundSMS' - Allows or denies outbound SMS messaging for the specified phone number.
newPhoneNumberCapabilities ::
  PhoneNumberCapabilities
newPhoneNumberCapabilities =
  PhoneNumberCapabilities'
    { inboundCall =
        Prelude.Nothing,
      inboundMMS = Prelude.Nothing,
      inboundSMS = Prelude.Nothing,
      outboundCall = Prelude.Nothing,
      outboundMMS = Prelude.Nothing,
      outboundSMS = Prelude.Nothing
    }

-- | Allows or denies inbound calling for the specified phone number.
phoneNumberCapabilities_inboundCall :: Lens.Lens' PhoneNumberCapabilities (Prelude.Maybe Prelude.Bool)
phoneNumberCapabilities_inboundCall = Lens.lens (\PhoneNumberCapabilities' {inboundCall} -> inboundCall) (\s@PhoneNumberCapabilities' {} a -> s {inboundCall = a} :: PhoneNumberCapabilities)

-- | Allows or denies inbound MMS messaging for the specified phone number.
phoneNumberCapabilities_inboundMMS :: Lens.Lens' PhoneNumberCapabilities (Prelude.Maybe Prelude.Bool)
phoneNumberCapabilities_inboundMMS = Lens.lens (\PhoneNumberCapabilities' {inboundMMS} -> inboundMMS) (\s@PhoneNumberCapabilities' {} a -> s {inboundMMS = a} :: PhoneNumberCapabilities)

-- | Allows or denies inbound SMS messaging for the specified phone number.
phoneNumberCapabilities_inboundSMS :: Lens.Lens' PhoneNumberCapabilities (Prelude.Maybe Prelude.Bool)
phoneNumberCapabilities_inboundSMS = Lens.lens (\PhoneNumberCapabilities' {inboundSMS} -> inboundSMS) (\s@PhoneNumberCapabilities' {} a -> s {inboundSMS = a} :: PhoneNumberCapabilities)

-- | Allows or denies outbound calling for the specified phone number.
phoneNumberCapabilities_outboundCall :: Lens.Lens' PhoneNumberCapabilities (Prelude.Maybe Prelude.Bool)
phoneNumberCapabilities_outboundCall = Lens.lens (\PhoneNumberCapabilities' {outboundCall} -> outboundCall) (\s@PhoneNumberCapabilities' {} a -> s {outboundCall = a} :: PhoneNumberCapabilities)

-- | Allows or denies outbound MMS messaging for the specified phone number.
phoneNumberCapabilities_outboundMMS :: Lens.Lens' PhoneNumberCapabilities (Prelude.Maybe Prelude.Bool)
phoneNumberCapabilities_outboundMMS = Lens.lens (\PhoneNumberCapabilities' {outboundMMS} -> outboundMMS) (\s@PhoneNumberCapabilities' {} a -> s {outboundMMS = a} :: PhoneNumberCapabilities)

-- | Allows or denies outbound SMS messaging for the specified phone number.
phoneNumberCapabilities_outboundSMS :: Lens.Lens' PhoneNumberCapabilities (Prelude.Maybe Prelude.Bool)
phoneNumberCapabilities_outboundSMS = Lens.lens (\PhoneNumberCapabilities' {outboundSMS} -> outboundSMS) (\s@PhoneNumberCapabilities' {} a -> s {outboundSMS = a} :: PhoneNumberCapabilities)

instance Data.FromJSON PhoneNumberCapabilities where
  parseJSON =
    Data.withObject
      "PhoneNumberCapabilities"
      ( \x ->
          PhoneNumberCapabilities'
            Prelude.<$> (x Data..:? "InboundCall")
            Prelude.<*> (x Data..:? "InboundMMS")
            Prelude.<*> (x Data..:? "InboundSMS")
            Prelude.<*> (x Data..:? "OutboundCall")
            Prelude.<*> (x Data..:? "OutboundMMS")
            Prelude.<*> (x Data..:? "OutboundSMS")
      )

instance Prelude.Hashable PhoneNumberCapabilities where
  hashWithSalt _salt PhoneNumberCapabilities' {..} =
    _salt `Prelude.hashWithSalt` inboundCall
      `Prelude.hashWithSalt` inboundMMS
      `Prelude.hashWithSalt` inboundSMS
      `Prelude.hashWithSalt` outboundCall
      `Prelude.hashWithSalt` outboundMMS
      `Prelude.hashWithSalt` outboundSMS

instance Prelude.NFData PhoneNumberCapabilities where
  rnf PhoneNumberCapabilities' {..} =
    Prelude.rnf inboundCall
      `Prelude.seq` Prelude.rnf inboundMMS
      `Prelude.seq` Prelude.rnf inboundSMS
      `Prelude.seq` Prelude.rnf outboundCall
      `Prelude.seq` Prelude.rnf outboundMMS
      `Prelude.seq` Prelude.rnf outboundSMS
