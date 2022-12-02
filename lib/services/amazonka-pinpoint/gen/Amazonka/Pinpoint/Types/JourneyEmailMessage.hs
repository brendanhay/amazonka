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
-- Module      : Amazonka.Pinpoint.Types.JourneyEmailMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.JourneyEmailMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the \"From\" address for an email message that\'s sent to
-- participants in a journey.
--
-- /See:/ 'newJourneyEmailMessage' smart constructor.
data JourneyEmailMessage = JourneyEmailMessage'
  { -- | The verified email address to send the email message from. The default
    -- address is the FromAddress specified for the email channel for the
    -- application.
    fromAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JourneyEmailMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fromAddress', 'journeyEmailMessage_fromAddress' - The verified email address to send the email message from. The default
-- address is the FromAddress specified for the email channel for the
-- application.
newJourneyEmailMessage ::
  JourneyEmailMessage
newJourneyEmailMessage =
  JourneyEmailMessage' {fromAddress = Prelude.Nothing}

-- | The verified email address to send the email message from. The default
-- address is the FromAddress specified for the email channel for the
-- application.
journeyEmailMessage_fromAddress :: Lens.Lens' JourneyEmailMessage (Prelude.Maybe Prelude.Text)
journeyEmailMessage_fromAddress = Lens.lens (\JourneyEmailMessage' {fromAddress} -> fromAddress) (\s@JourneyEmailMessage' {} a -> s {fromAddress = a} :: JourneyEmailMessage)

instance Data.FromJSON JourneyEmailMessage where
  parseJSON =
    Data.withObject
      "JourneyEmailMessage"
      ( \x ->
          JourneyEmailMessage'
            Prelude.<$> (x Data..:? "FromAddress")
      )

instance Prelude.Hashable JourneyEmailMessage where
  hashWithSalt _salt JourneyEmailMessage' {..} =
    _salt `Prelude.hashWithSalt` fromAddress

instance Prelude.NFData JourneyEmailMessage where
  rnf JourneyEmailMessage' {..} =
    Prelude.rnf fromAddress

instance Data.ToJSON JourneyEmailMessage where
  toJSON JourneyEmailMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [("FromAddress" Data..=) Prelude.<$> fromAddress]
      )
