{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filter used in the request. Supported filter keys are Name and Enabled.
--
--
--
-- /See:/ 'maintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { _mwfValues ::
      !(Maybe [Text]),
    _mwfKey :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwfValues' - The filter values.
--
-- * 'mwfKey' - The name of the filter.
maintenanceWindowFilter ::
  MaintenanceWindowFilter
maintenanceWindowFilter =
  MaintenanceWindowFilter' {_mwfValues = Nothing, _mwfKey = Nothing}

-- | The filter values.
mwfValues :: Lens' MaintenanceWindowFilter [Text]
mwfValues = lens _mwfValues (\s a -> s {_mwfValues = a}) . _Default . _Coerce

-- | The name of the filter.
mwfKey :: Lens' MaintenanceWindowFilter (Maybe Text)
mwfKey = lens _mwfKey (\s a -> s {_mwfKey = a})

instance Hashable MaintenanceWindowFilter

instance NFData MaintenanceWindowFilter

instance ToJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter' {..} =
    object
      (catMaybes [("Values" .=) <$> _mwfValues, ("Key" .=) <$> _mwfKey])
