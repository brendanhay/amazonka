{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.QueryFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.QueryFilter where

import Network.AWS.IoTAnalytics.Types.DeltaTime
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information that is used to filter message data, to segregate it according to the timeframe in which it arrives.
--
--
--
-- /See:/ 'queryFilter' smart constructor.
newtype QueryFilter = QueryFilter' {_qfDeltaTime :: Maybe DeltaTime}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'QueryFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'qfDeltaTime' - Used to limit data to that which has arrived since the last execution of the action.
queryFilter ::
  QueryFilter
queryFilter = QueryFilter' {_qfDeltaTime = Nothing}

-- | Used to limit data to that which has arrived since the last execution of the action.
qfDeltaTime :: Lens' QueryFilter (Maybe DeltaTime)
qfDeltaTime = lens _qfDeltaTime (\s a -> s {_qfDeltaTime = a})

instance FromJSON QueryFilter where
  parseJSON =
    withObject
      "QueryFilter"
      (\x -> QueryFilter' <$> (x .:? "deltaTime"))

instance Hashable QueryFilter

instance NFData QueryFilter

instance ToJSON QueryFilter where
  toJSON QueryFilter' {..} =
    object (catMaybes [("deltaTime" .=) <$> _qfDeltaTime])
