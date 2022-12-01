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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.OptedOutNumberInformation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.OptedOutNumberInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information for an opted out number in an Amazon Web Services
-- account.
--
-- /See:/ 'newOptedOutNumberInformation' smart constructor.
data OptedOutNumberInformation = OptedOutNumberInformation'
  { -- | The phone number that is opted out.
    optedOutNumber :: Prelude.Text,
    -- | The time that the op tout occurred, in
    -- <https://www.epochconverter.com/ UNIX epoch time> format.
    optedOutTimestamp :: Core.POSIX,
    -- | This is set to true if it was the end recipient that opted out.
    endUserOptedOut :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptedOutNumberInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optedOutNumber', 'optedOutNumberInformation_optedOutNumber' - The phone number that is opted out.
--
-- 'optedOutTimestamp', 'optedOutNumberInformation_optedOutTimestamp' - The time that the op tout occurred, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
--
-- 'endUserOptedOut', 'optedOutNumberInformation_endUserOptedOut' - This is set to true if it was the end recipient that opted out.
newOptedOutNumberInformation ::
  -- | 'optedOutNumber'
  Prelude.Text ->
  -- | 'optedOutTimestamp'
  Prelude.UTCTime ->
  -- | 'endUserOptedOut'
  Prelude.Bool ->
  OptedOutNumberInformation
newOptedOutNumberInformation
  pOptedOutNumber_
  pOptedOutTimestamp_
  pEndUserOptedOut_ =
    OptedOutNumberInformation'
      { optedOutNumber =
          pOptedOutNumber_,
        optedOutTimestamp =
          Core._Time Lens.# pOptedOutTimestamp_,
        endUserOptedOut = pEndUserOptedOut_
      }

-- | The phone number that is opted out.
optedOutNumberInformation_optedOutNumber :: Lens.Lens' OptedOutNumberInformation Prelude.Text
optedOutNumberInformation_optedOutNumber = Lens.lens (\OptedOutNumberInformation' {optedOutNumber} -> optedOutNumber) (\s@OptedOutNumberInformation' {} a -> s {optedOutNumber = a} :: OptedOutNumberInformation)

-- | The time that the op tout occurred, in
-- <https://www.epochconverter.com/ UNIX epoch time> format.
optedOutNumberInformation_optedOutTimestamp :: Lens.Lens' OptedOutNumberInformation Prelude.UTCTime
optedOutNumberInformation_optedOutTimestamp = Lens.lens (\OptedOutNumberInformation' {optedOutTimestamp} -> optedOutTimestamp) (\s@OptedOutNumberInformation' {} a -> s {optedOutTimestamp = a} :: OptedOutNumberInformation) Prelude.. Core._Time

-- | This is set to true if it was the end recipient that opted out.
optedOutNumberInformation_endUserOptedOut :: Lens.Lens' OptedOutNumberInformation Prelude.Bool
optedOutNumberInformation_endUserOptedOut = Lens.lens (\OptedOutNumberInformation' {endUserOptedOut} -> endUserOptedOut) (\s@OptedOutNumberInformation' {} a -> s {endUserOptedOut = a} :: OptedOutNumberInformation)

instance Core.FromJSON OptedOutNumberInformation where
  parseJSON =
    Core.withObject
      "OptedOutNumberInformation"
      ( \x ->
          OptedOutNumberInformation'
            Prelude.<$> (x Core..: "OptedOutNumber")
            Prelude.<*> (x Core..: "OptedOutTimestamp")
            Prelude.<*> (x Core..: "EndUserOptedOut")
      )

instance Prelude.Hashable OptedOutNumberInformation where
  hashWithSalt _salt OptedOutNumberInformation' {..} =
    _salt `Prelude.hashWithSalt` optedOutNumber
      `Prelude.hashWithSalt` optedOutTimestamp
      `Prelude.hashWithSalt` endUserOptedOut

instance Prelude.NFData OptedOutNumberInformation where
  rnf OptedOutNumberInformation' {..} =
    Prelude.rnf optedOutNumber
      `Prelude.seq` Prelude.rnf optedOutTimestamp
      `Prelude.seq` Prelude.rnf endUserOptedOut
