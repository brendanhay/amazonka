{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAggregatedAssociationOverview where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status information about the aggregated associations.
--
--
--
-- /See:/ 'instanceAggregatedAssociationOverview' smart constructor.
data InstanceAggregatedAssociationOverview = InstanceAggregatedAssociationOverview'
  { _iaaoDetailedStatus ::
      !(Maybe Text),
    _iaaoInstanceAssociationStatusAggregatedCount ::
      !( Maybe
           ( Map
               Text
               (Int)
           )
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceAggregatedAssociationOverview' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaaoDetailedStatus' - Detailed status information about the aggregated associations.
--
-- * 'iaaoInstanceAssociationStatusAggregatedCount' - The number of associations for the instance(s).
instanceAggregatedAssociationOverview ::
  InstanceAggregatedAssociationOverview
instanceAggregatedAssociationOverview =
  InstanceAggregatedAssociationOverview'
    { _iaaoDetailedStatus =
        Nothing,
      _iaaoInstanceAssociationStatusAggregatedCount = Nothing
    }

-- | Detailed status information about the aggregated associations.
iaaoDetailedStatus :: Lens' InstanceAggregatedAssociationOverview (Maybe Text)
iaaoDetailedStatus = lens _iaaoDetailedStatus (\s a -> s {_iaaoDetailedStatus = a})

-- | The number of associations for the instance(s).
iaaoInstanceAssociationStatusAggregatedCount :: Lens' InstanceAggregatedAssociationOverview (HashMap Text (Int))
iaaoInstanceAssociationStatusAggregatedCount = lens _iaaoInstanceAssociationStatusAggregatedCount (\s a -> s {_iaaoInstanceAssociationStatusAggregatedCount = a}) . _Default . _Map

instance FromJSON InstanceAggregatedAssociationOverview where
  parseJSON =
    withObject
      "InstanceAggregatedAssociationOverview"
      ( \x ->
          InstanceAggregatedAssociationOverview'
            <$> (x .:? "DetailedStatus")
            <*> (x .:? "InstanceAssociationStatusAggregatedCount" .!= mempty)
      )

instance Hashable InstanceAggregatedAssociationOverview

instance NFData InstanceAggregatedAssociationOverview
