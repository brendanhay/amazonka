{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DeltaTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DeltaTime where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used to limit data to that which has arrived since the last execution of the action.
--
--
--
-- /See:/ 'deltaTime' smart constructor.
data DeltaTime = DeltaTime'
  { _dtOffsetSeconds :: !Int,
    _dtTimeExpression :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeltaTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtOffsetSeconds' - The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
--
-- * 'dtTimeExpression' - An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
deltaTime ::
  -- | 'dtOffsetSeconds'
  Int ->
  -- | 'dtTimeExpression'
  Text ->
  DeltaTime
deltaTime pOffsetSeconds_ pTimeExpression_ =
  DeltaTime'
    { _dtOffsetSeconds = pOffsetSeconds_,
      _dtTimeExpression = pTimeExpression_
    }

-- | The number of seconds of estimated in-flight lag time of message data. When you create dataset contents using message data from a specified timeframe, some message data might still be in flight when processing begins, and so do not arrive in time to be processed. Use this field to make allowances for the in flight time of your message data, so that data not processed from a previous timeframe is included with the next timeframe. Otherwise, missed message data would be excluded from processing during the next timeframe too, because its timestamp places it within the previous timeframe.
dtOffsetSeconds :: Lens' DeltaTime Int
dtOffsetSeconds = lens _dtOffsetSeconds (\s a -> s {_dtOffsetSeconds = a})

-- | An expression by which the time of the message data might be determined. This can be the name of a timestamp field or a SQL expression that is used to derive the time the message data was generated.
dtTimeExpression :: Lens' DeltaTime Text
dtTimeExpression = lens _dtTimeExpression (\s a -> s {_dtTimeExpression = a})

instance FromJSON DeltaTime where
  parseJSON =
    withObject
      "DeltaTime"
      ( \x ->
          DeltaTime' <$> (x .: "offsetSeconds") <*> (x .: "timeExpression")
      )

instance Hashable DeltaTime

instance NFData DeltaTime

instance ToJSON DeltaTime where
  toJSON DeltaTime' {..} =
    object
      ( catMaybes
          [ Just ("offsetSeconds" .= _dtOffsetSeconds),
            Just ("timeExpression" .= _dtTimeExpression)
          ]
      )
