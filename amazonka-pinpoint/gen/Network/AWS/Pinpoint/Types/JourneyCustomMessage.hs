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
-- Module      : Network.AWS.Pinpoint.Types.JourneyCustomMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneyCustomMessage where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the message content for a custom channel message that\'s sent
-- to participants in a journey.
--
-- /See:/ 'newJourneyCustomMessage' smart constructor.
data JourneyCustomMessage = JourneyCustomMessage'
  { -- | The message content that\'s passed to an AWS Lambda function or to a web
    -- hook.
    data' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JourneyCustomMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'journeyCustomMessage_data' - The message content that\'s passed to an AWS Lambda function or to a web
-- hook.
newJourneyCustomMessage ::
  JourneyCustomMessage
newJourneyCustomMessage =
  JourneyCustomMessage' {data' = Prelude.Nothing}

-- | The message content that\'s passed to an AWS Lambda function or to a web
-- hook.
journeyCustomMessage_data :: Lens.Lens' JourneyCustomMessage (Prelude.Maybe Prelude.Text)
journeyCustomMessage_data = Lens.lens (\JourneyCustomMessage' {data'} -> data') (\s@JourneyCustomMessage' {} a -> s {data' = a} :: JourneyCustomMessage)

instance Prelude.FromJSON JourneyCustomMessage where
  parseJSON =
    Prelude.withObject
      "JourneyCustomMessage"
      ( \x ->
          JourneyCustomMessage'
            Prelude.<$> (x Prelude..:? "Data")
      )

instance Prelude.Hashable JourneyCustomMessage

instance Prelude.NFData JourneyCustomMessage

instance Prelude.ToJSON JourneyCustomMessage where
  toJSON JourneyCustomMessage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Data" Prelude..=) Prelude.<$> data']
      )
