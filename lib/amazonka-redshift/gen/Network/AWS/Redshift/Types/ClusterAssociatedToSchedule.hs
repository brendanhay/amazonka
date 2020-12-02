{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterAssociatedToSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterAssociatedToSchedule where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.ScheduleState

-- |
--
--
--
-- /See:/ 'clusterAssociatedToSchedule' smart constructor.
data ClusterAssociatedToSchedule = ClusterAssociatedToSchedule'
  { _catsScheduleAssociationState ::
      !(Maybe ScheduleState),
    _catsClusterIdentifier ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterAssociatedToSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'catsScheduleAssociationState' -
--
-- * 'catsClusterIdentifier' -
clusterAssociatedToSchedule ::
  ClusterAssociatedToSchedule
clusterAssociatedToSchedule =
  ClusterAssociatedToSchedule'
    { _catsScheduleAssociationState =
        Nothing,
      _catsClusterIdentifier = Nothing
    }

-- |
catsScheduleAssociationState :: Lens' ClusterAssociatedToSchedule (Maybe ScheduleState)
catsScheduleAssociationState = lens _catsScheduleAssociationState (\s a -> s {_catsScheduleAssociationState = a})

-- |
catsClusterIdentifier :: Lens' ClusterAssociatedToSchedule (Maybe Text)
catsClusterIdentifier = lens _catsClusterIdentifier (\s a -> s {_catsClusterIdentifier = a})

instance FromXML ClusterAssociatedToSchedule where
  parseXML x =
    ClusterAssociatedToSchedule'
      <$> (x .@? "ScheduleAssociationState") <*> (x .@? "ClusterIdentifier")

instance Hashable ClusterAssociatedToSchedule

instance NFData ClusterAssociatedToSchedule
