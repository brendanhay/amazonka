{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ReservationCoverageGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ReservationCoverageGroup where

import Network.AWS.CostExplorer.Types.Coverage
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A group of reservations that share a set of attributes.
--
--
--
-- /See:/ 'reservationCoverageGroup' smart constructor.
data ReservationCoverageGroup = ReservationCoverageGroup'
  { _rcgCoverage ::
      !(Maybe Coverage),
    _rcgAttributes ::
      !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReservationCoverageGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcgCoverage' - How much instance usage this group of reservations covered.
--
-- * 'rcgAttributes' - The attributes for this group of reservations.
reservationCoverageGroup ::
  ReservationCoverageGroup
reservationCoverageGroup =
  ReservationCoverageGroup'
    { _rcgCoverage = Nothing,
      _rcgAttributes = Nothing
    }

-- | How much instance usage this group of reservations covered.
rcgCoverage :: Lens' ReservationCoverageGroup (Maybe Coverage)
rcgCoverage = lens _rcgCoverage (\s a -> s {_rcgCoverage = a})

-- | The attributes for this group of reservations.
rcgAttributes :: Lens' ReservationCoverageGroup (HashMap Text (Text))
rcgAttributes = lens _rcgAttributes (\s a -> s {_rcgAttributes = a}) . _Default . _Map

instance FromJSON ReservationCoverageGroup where
  parseJSON =
    withObject
      "ReservationCoverageGroup"
      ( \x ->
          ReservationCoverageGroup'
            <$> (x .:? "Coverage") <*> (x .:? "Attributes" .!= mempty)
      )

instance Hashable ReservationCoverageGroup

instance NFData ReservationCoverageGroup
