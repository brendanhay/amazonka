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
-- Module      : Network.AWS.Snowball.Types.EventTriggerDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EventTriggerDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The container for the EventTriggerDefinition$EventResourceARN.
--
-- /See:/ 'newEventTriggerDefinition' smart constructor.
data EventTriggerDefinition = EventTriggerDefinition'
  { -- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is
    -- an AWS Lambda function\'s event trigger associated with this job.
    eventResourceARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventTriggerDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventResourceARN', 'eventTriggerDefinition_eventResourceARN' - The Amazon Resource Name (ARN) for any local Amazon S3 resource that is
-- an AWS Lambda function\'s event trigger associated with this job.
newEventTriggerDefinition ::
  EventTriggerDefinition
newEventTriggerDefinition =
  EventTriggerDefinition'
    { eventResourceARN =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for any local Amazon S3 resource that is
-- an AWS Lambda function\'s event trigger associated with this job.
eventTriggerDefinition_eventResourceARN :: Lens.Lens' EventTriggerDefinition (Prelude.Maybe Prelude.Text)
eventTriggerDefinition_eventResourceARN = Lens.lens (\EventTriggerDefinition' {eventResourceARN} -> eventResourceARN) (\s@EventTriggerDefinition' {} a -> s {eventResourceARN = a} :: EventTriggerDefinition)

instance Prelude.FromJSON EventTriggerDefinition where
  parseJSON =
    Prelude.withObject
      "EventTriggerDefinition"
      ( \x ->
          EventTriggerDefinition'
            Prelude.<$> (x Prelude..:? "EventResourceARN")
      )

instance Prelude.Hashable EventTriggerDefinition

instance Prelude.NFData EventTriggerDefinition

instance Prelude.ToJSON EventTriggerDefinition where
  toJSON EventTriggerDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EventResourceARN" Prelude..=)
              Prelude.<$> eventResourceARN
          ]
      )
