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
-- Module      : Network.AWS.Pinpoint.Types.JourneyEmailMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyEmailMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON JourneyEmailMessage where
  parseJSON =
    Prelude.withObject
      "JourneyEmailMessage"
      ( \x ->
          JourneyEmailMessage'
            Prelude.<$> (x Prelude..:? "FromAddress")
      )

instance Prelude.Hashable JourneyEmailMessage

instance Prelude.NFData JourneyEmailMessage

instance Prelude.ToJSON JourneyEmailMessage where
  toJSON JourneyEmailMessage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("FromAddress" Prelude..=) Prelude.<$> fromAddress]
      )
