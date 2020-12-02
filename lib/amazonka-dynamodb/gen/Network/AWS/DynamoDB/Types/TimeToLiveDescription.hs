{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveDescription where

import Network.AWS.DynamoDB.Types.TimeToLiveStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of the Time to Live (TTL) status on the specified table.
--
--
--
-- /See:/ 'timeToLiveDescription' smart constructor.
data TimeToLiveDescription = TimeToLiveDescription'
  { _ttldTimeToLiveStatus ::
      !(Maybe TimeToLiveStatus),
    _ttldAttributeName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeToLiveDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ttldTimeToLiveStatus' - The TTL status for the table.
--
-- * 'ttldAttributeName' - The name of the TTL attribute for items in the table.
timeToLiveDescription ::
  TimeToLiveDescription
timeToLiveDescription =
  TimeToLiveDescription'
    { _ttldTimeToLiveStatus = Nothing,
      _ttldAttributeName = Nothing
    }

-- | The TTL status for the table.
ttldTimeToLiveStatus :: Lens' TimeToLiveDescription (Maybe TimeToLiveStatus)
ttldTimeToLiveStatus = lens _ttldTimeToLiveStatus (\s a -> s {_ttldTimeToLiveStatus = a})

-- | The name of the TTL attribute for items in the table.
ttldAttributeName :: Lens' TimeToLiveDescription (Maybe Text)
ttldAttributeName = lens _ttldAttributeName (\s a -> s {_ttldAttributeName = a})

instance FromJSON TimeToLiveDescription where
  parseJSON =
    withObject
      "TimeToLiveDescription"
      ( \x ->
          TimeToLiveDescription'
            <$> (x .:? "TimeToLiveStatus") <*> (x .:? "AttributeName")
      )

instance Hashable TimeToLiveDescription

instance NFData TimeToLiveDescription
