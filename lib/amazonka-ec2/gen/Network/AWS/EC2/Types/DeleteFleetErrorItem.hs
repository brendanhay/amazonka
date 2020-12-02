{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetErrorItem where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteFleetError
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an EC2 Fleet that was not successfully deleted.
--
--
--
-- /See:/ 'deleteFleetErrorItem' smart constructor.
data DeleteFleetErrorItem = DeleteFleetErrorItem'
  { _dfeiError ::
      !(Maybe DeleteFleetError),
    _dfeiFleetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteFleetErrorItem' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfeiError' - The error.
--
-- * 'dfeiFleetId' - The ID of the EC2 Fleet.
deleteFleetErrorItem ::
  DeleteFleetErrorItem
deleteFleetErrorItem =
  DeleteFleetErrorItem'
    { _dfeiError = Nothing,
      _dfeiFleetId = Nothing
    }

-- | The error.
dfeiError :: Lens' DeleteFleetErrorItem (Maybe DeleteFleetError)
dfeiError = lens _dfeiError (\s a -> s {_dfeiError = a})

-- | The ID of the EC2 Fleet.
dfeiFleetId :: Lens' DeleteFleetErrorItem (Maybe Text)
dfeiFleetId = lens _dfeiFleetId (\s a -> s {_dfeiFleetId = a})

instance FromXML DeleteFleetErrorItem where
  parseXML x =
    DeleteFleetErrorItem' <$> (x .@? "error") <*> (x .@? "fleetId")

instance Hashable DeleteFleetErrorItem

instance NFData DeleteFleetErrorItem
