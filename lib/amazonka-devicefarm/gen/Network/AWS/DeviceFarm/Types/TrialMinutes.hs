{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TrialMinutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TrialMinutes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about free trial device minutes for an AWS account.
--
--
--
-- /See:/ 'trialMinutes' smart constructor.
data TrialMinutes = TrialMinutes'
  { _tmRemaining :: !(Maybe Double),
    _tmTotal :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TrialMinutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmRemaining' - The number of free trial minutes remaining in the account.
--
-- * 'tmTotal' - The total number of free trial minutes that the account started with.
trialMinutes ::
  TrialMinutes
trialMinutes =
  TrialMinutes' {_tmRemaining = Nothing, _tmTotal = Nothing}

-- | The number of free trial minutes remaining in the account.
tmRemaining :: Lens' TrialMinutes (Maybe Double)
tmRemaining = lens _tmRemaining (\s a -> s {_tmRemaining = a})

-- | The total number of free trial minutes that the account started with.
tmTotal :: Lens' TrialMinutes (Maybe Double)
tmTotal = lens _tmTotal (\s a -> s {_tmTotal = a})

instance FromJSON TrialMinutes where
  parseJSON =
    withObject
      "TrialMinutes"
      (\x -> TrialMinutes' <$> (x .:? "remaining") <*> (x .:? "total"))

instance Hashable TrialMinutes

instance NFData TrialMinutes
