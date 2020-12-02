{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TriggerNodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TriggerNodeDetails where

import Network.AWS.Glue.Types.Trigger
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The details of a Trigger node present in the workflow.
--
--
--
-- /See:/ 'triggerNodeDetails' smart constructor.
newtype TriggerNodeDetails = TriggerNodeDetails'
  { _tndTrigger ::
      Maybe Trigger
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TriggerNodeDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tndTrigger' - The information of the trigger represented by the trigger node.
triggerNodeDetails ::
  TriggerNodeDetails
triggerNodeDetails = TriggerNodeDetails' {_tndTrigger = Nothing}

-- | The information of the trigger represented by the trigger node.
tndTrigger :: Lens' TriggerNodeDetails (Maybe Trigger)
tndTrigger = lens _tndTrigger (\s a -> s {_tndTrigger = a})

instance FromJSON TriggerNodeDetails where
  parseJSON =
    withObject
      "TriggerNodeDetails"
      (\x -> TriggerNodeDetails' <$> (x .:? "Trigger"))

instance Hashable TriggerNodeDetails

instance NFData TriggerNodeDetails
