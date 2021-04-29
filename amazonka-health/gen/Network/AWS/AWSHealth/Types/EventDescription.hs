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
-- Module      : Network.AWS.AWSHealth.Types.EventDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.EventDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The detailed description of the event. Included in the information
-- returned by the
-- <https://docs.aws.amazon.com/health/latest/APIReference/API_DescribeEventDetails.html DescribeEventDetails>
-- operation.
--
-- /See:/ 'newEventDescription' smart constructor.
data EventDescription = EventDescription'
  { -- | The most recent description of the event.
    latestDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestDescription', 'eventDescription_latestDescription' - The most recent description of the event.
newEventDescription ::
  EventDescription
newEventDescription =
  EventDescription'
    { latestDescription =
        Prelude.Nothing
    }

-- | The most recent description of the event.
eventDescription_latestDescription :: Lens.Lens' EventDescription (Prelude.Maybe Prelude.Text)
eventDescription_latestDescription = Lens.lens (\EventDescription' {latestDescription} -> latestDescription) (\s@EventDescription' {} a -> s {latestDescription = a} :: EventDescription)

instance Prelude.FromJSON EventDescription where
  parseJSON =
    Prelude.withObject
      "EventDescription"
      ( \x ->
          EventDescription'
            Prelude.<$> (x Prelude..:? "latestDescription")
      )

instance Prelude.Hashable EventDescription

instance Prelude.NFData EventDescription
