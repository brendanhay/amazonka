{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelCounters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCounters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a breakdown of the number of objects labeled.
--
--
--
-- /See:/ 'labelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { _lcMachineLabeled ::
      !(Maybe Nat),
    _lcTotalLabeled :: !(Maybe Nat),
    _lcFailedNonRetryableError :: !(Maybe Nat),
    _lcUnlabeled :: !(Maybe Nat),
    _lcHumanLabeled :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelCounters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcMachineLabeled' - The total number of objects labeled by automated data labeling.
--
-- * 'lcTotalLabeled' - The total number of objects labeled.
--
-- * 'lcFailedNonRetryableError' - The total number of objects that could not be labeled due to an error.
--
-- * 'lcUnlabeled' - The total number of objects not yet labeled.
--
-- * 'lcHumanLabeled' - The total number of objects labeled by a human worker.
labelCounters ::
  LabelCounters
labelCounters =
  LabelCounters'
    { _lcMachineLabeled = Nothing,
      _lcTotalLabeled = Nothing,
      _lcFailedNonRetryableError = Nothing,
      _lcUnlabeled = Nothing,
      _lcHumanLabeled = Nothing
    }

-- | The total number of objects labeled by automated data labeling.
lcMachineLabeled :: Lens' LabelCounters (Maybe Natural)
lcMachineLabeled = lens _lcMachineLabeled (\s a -> s {_lcMachineLabeled = a}) . mapping _Nat

-- | The total number of objects labeled.
lcTotalLabeled :: Lens' LabelCounters (Maybe Natural)
lcTotalLabeled = lens _lcTotalLabeled (\s a -> s {_lcTotalLabeled = a}) . mapping _Nat

-- | The total number of objects that could not be labeled due to an error.
lcFailedNonRetryableError :: Lens' LabelCounters (Maybe Natural)
lcFailedNonRetryableError = lens _lcFailedNonRetryableError (\s a -> s {_lcFailedNonRetryableError = a}) . mapping _Nat

-- | The total number of objects not yet labeled.
lcUnlabeled :: Lens' LabelCounters (Maybe Natural)
lcUnlabeled = lens _lcUnlabeled (\s a -> s {_lcUnlabeled = a}) . mapping _Nat

-- | The total number of objects labeled by a human worker.
lcHumanLabeled :: Lens' LabelCounters (Maybe Natural)
lcHumanLabeled = lens _lcHumanLabeled (\s a -> s {_lcHumanLabeled = a}) . mapping _Nat

instance FromJSON LabelCounters where
  parseJSON =
    withObject
      "LabelCounters"
      ( \x ->
          LabelCounters'
            <$> (x .:? "MachineLabeled")
            <*> (x .:? "TotalLabeled")
            <*> (x .:? "FailedNonRetryableError")
            <*> (x .:? "Unlabeled")
            <*> (x .:? "HumanLabeled")
      )

instance Hashable LabelCounters

instance NFData LabelCounters
