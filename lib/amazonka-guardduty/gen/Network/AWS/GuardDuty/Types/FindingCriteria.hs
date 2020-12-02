{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingCriteria
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingCriteria where

import Network.AWS.GuardDuty.Types.Condition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the criteria used for querying findings.
--
--
--
-- /See:/ 'findingCriteria' smart constructor.
newtype FindingCriteria = FindingCriteria'
  { _fcCriterion ::
      Maybe (Map Text (Condition))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FindingCriteria' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcCriterion' - Represents a map of finding properties that match specified conditions and values when querying findings.
findingCriteria ::
  FindingCriteria
findingCriteria = FindingCriteria' {_fcCriterion = Nothing}

-- | Represents a map of finding properties that match specified conditions and values when querying findings.
fcCriterion :: Lens' FindingCriteria (HashMap Text (Condition))
fcCriterion = lens _fcCriterion (\s a -> s {_fcCriterion = a}) . _Default . _Map

instance FromJSON FindingCriteria where
  parseJSON =
    withObject
      "FindingCriteria"
      (\x -> FindingCriteria' <$> (x .:? "criterion" .!= mempty))

instance Hashable FindingCriteria

instance NFData FindingCriteria

instance ToJSON FindingCriteria where
  toJSON FindingCriteria' {..} =
    object (catMaybes [("criterion" .=) <$> _fcCriterion])
