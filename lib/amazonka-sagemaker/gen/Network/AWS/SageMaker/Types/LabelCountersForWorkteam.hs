{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelCountersForWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCountersForWorkteam where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides counts for human-labeled tasks in the labeling job.
--
--
--
-- /See:/ 'labelCountersForWorkteam' smart constructor.
data LabelCountersForWorkteam = LabelCountersForWorkteam'
  { _lcfwPendingHuman ::
      !(Maybe Nat),
    _lcfwTotal :: !(Maybe Nat),
    _lcfwHumanLabeled :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelCountersForWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfwPendingHuman' - The total number of data objects that need to be labeled by a human worker.
--
-- * 'lcfwTotal' - The total number of tasks in the labeling job.
--
-- * 'lcfwHumanLabeled' - The total number of data objects labeled by a human worker.
labelCountersForWorkteam ::
  LabelCountersForWorkteam
labelCountersForWorkteam =
  LabelCountersForWorkteam'
    { _lcfwPendingHuman = Nothing,
      _lcfwTotal = Nothing,
      _lcfwHumanLabeled = Nothing
    }

-- | The total number of data objects that need to be labeled by a human worker.
lcfwPendingHuman :: Lens' LabelCountersForWorkteam (Maybe Natural)
lcfwPendingHuman = lens _lcfwPendingHuman (\s a -> s {_lcfwPendingHuman = a}) . mapping _Nat

-- | The total number of tasks in the labeling job.
lcfwTotal :: Lens' LabelCountersForWorkteam (Maybe Natural)
lcfwTotal = lens _lcfwTotal (\s a -> s {_lcfwTotal = a}) . mapping _Nat

-- | The total number of data objects labeled by a human worker.
lcfwHumanLabeled :: Lens' LabelCountersForWorkteam (Maybe Natural)
lcfwHumanLabeled = lens _lcfwHumanLabeled (\s a -> s {_lcfwHumanLabeled = a}) . mapping _Nat

instance FromJSON LabelCountersForWorkteam where
  parseJSON =
    withObject
      "LabelCountersForWorkteam"
      ( \x ->
          LabelCountersForWorkteam'
            <$> (x .:? "PendingHuman")
            <*> (x .:? "Total")
            <*> (x .:? "HumanLabeled")
      )

instance Hashable LabelCountersForWorkteam

instance NFData LabelCountersForWorkteam
