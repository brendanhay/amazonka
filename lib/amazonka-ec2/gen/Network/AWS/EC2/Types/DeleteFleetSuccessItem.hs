{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetSuccessItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetSuccessItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetStateCode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 Fleet that was successfully deleted.
--
--
--
-- /See:/ 'deleteFleetSuccessItem' smart constructor.
data DeleteFleetSuccessItem = DeleteFleetSuccessItem'
  { _dfsiCurrentFleetState ::
      !(Maybe FleetStateCode),
    _dfsiPreviousFleetState ::
      !(Maybe FleetStateCode),
    _dfsiFleetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFleetSuccessItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsiCurrentFleetState' - The current state of the EC2 Fleet.
--
-- * 'dfsiPreviousFleetState' - The previous state of the EC2 Fleet.
--
-- * 'dfsiFleetId' - The ID of the EC2 Fleet.
deleteFleetSuccessItem ::
  DeleteFleetSuccessItem
deleteFleetSuccessItem =
  DeleteFleetSuccessItem'
    { _dfsiCurrentFleetState = Nothing,
      _dfsiPreviousFleetState = Nothing,
      _dfsiFleetId = Nothing
    }

-- | The current state of the EC2 Fleet.
dfsiCurrentFleetState :: Lens' DeleteFleetSuccessItem (Maybe FleetStateCode)
dfsiCurrentFleetState = lens _dfsiCurrentFleetState (\s a -> s {_dfsiCurrentFleetState = a})

-- | The previous state of the EC2 Fleet.
dfsiPreviousFleetState :: Lens' DeleteFleetSuccessItem (Maybe FleetStateCode)
dfsiPreviousFleetState = lens _dfsiPreviousFleetState (\s a -> s {_dfsiPreviousFleetState = a})

-- | The ID of the EC2 Fleet.
dfsiFleetId :: Lens' DeleteFleetSuccessItem (Maybe Text)
dfsiFleetId = lens _dfsiFleetId (\s a -> s {_dfsiFleetId = a})

instance FromXML DeleteFleetSuccessItem where
  parseXML x =
    DeleteFleetSuccessItem'
      <$> (x .@? "currentFleetState")
      <*> (x .@? "previousFleetState")
      <*> (x .@? "fleetId")

instance Hashable DeleteFleetSuccessItem

instance NFData DeleteFleetSuccessItem
