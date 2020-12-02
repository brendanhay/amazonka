{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.TelemetryMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TelemetryMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The metadata about the Amazon Inspector application data metrics collected by the agent. This data type is used as the response element in the 'GetTelemetryMetadata' action.
--
--
--
-- /See:/ 'telemetryMetadata' smart constructor.
data TelemetryMetadata = TelemetryMetadata'
  { _tmDataSize ::
      !(Maybe Integer),
    _tmMessageType :: !Text,
    _tmCount :: !Integer
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TelemetryMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tmDataSize' - The data size of messages that the agent sends to the Amazon Inspector service.
--
-- * 'tmMessageType' - A specific type of behavioral data that is collected by the agent.
--
-- * 'tmCount' - The count of messages that the agent sends to the Amazon Inspector service.
telemetryMetadata ::
  -- | 'tmMessageType'
  Text ->
  -- | 'tmCount'
  Integer ->
  TelemetryMetadata
telemetryMetadata pMessageType_ pCount_ =
  TelemetryMetadata'
    { _tmDataSize = Nothing,
      _tmMessageType = pMessageType_,
      _tmCount = pCount_
    }

-- | The data size of messages that the agent sends to the Amazon Inspector service.
tmDataSize :: Lens' TelemetryMetadata (Maybe Integer)
tmDataSize = lens _tmDataSize (\s a -> s {_tmDataSize = a})

-- | A specific type of behavioral data that is collected by the agent.
tmMessageType :: Lens' TelemetryMetadata Text
tmMessageType = lens _tmMessageType (\s a -> s {_tmMessageType = a})

-- | The count of messages that the agent sends to the Amazon Inspector service.
tmCount :: Lens' TelemetryMetadata Integer
tmCount = lens _tmCount (\s a -> s {_tmCount = a})

instance FromJSON TelemetryMetadata where
  parseJSON =
    withObject
      "TelemetryMetadata"
      ( \x ->
          TelemetryMetadata'
            <$> (x .:? "dataSize") <*> (x .: "messageType") <*> (x .: "count")
      )

instance Hashable TelemetryMetadata

instance NFData TelemetryMetadata
