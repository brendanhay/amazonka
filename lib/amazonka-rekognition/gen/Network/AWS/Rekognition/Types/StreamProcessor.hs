{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.StreamProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.StreamProcessor where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types.StreamProcessorStatus

-- | An object that recognizes faces in a streaming video. An Amazon Rekognition stream processor is created by a call to 'CreateStreamProcessor' . The request parameters for @CreateStreamProcessor@ describe the Kinesis video stream source for the streaming video, face recognition parameters, and where to stream the analysis resullts.
--
--
--
-- /See:/ 'streamProcessor' smart constructor.
data StreamProcessor = StreamProcessor'
  { _spStatus ::
      !(Maybe StreamProcessorStatus),
    _spName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spStatus' - Current status of the Amazon Rekognition stream processor.
--
-- * 'spName' - Name of the Amazon Rekognition stream processor.
streamProcessor ::
  StreamProcessor
streamProcessor =
  StreamProcessor' {_spStatus = Nothing, _spName = Nothing}

-- | Current status of the Amazon Rekognition stream processor.
spStatus :: Lens' StreamProcessor (Maybe StreamProcessorStatus)
spStatus = lens _spStatus (\s a -> s {_spStatus = a})

-- | Name of the Amazon Rekognition stream processor.
spName :: Lens' StreamProcessor (Maybe Text)
spName = lens _spName (\s a -> s {_spName = a})

instance FromJSON StreamProcessor where
  parseJSON =
    withObject
      "StreamProcessor"
      (\x -> StreamProcessor' <$> (x .:? "Status") <*> (x .:? "Name"))

instance Hashable StreamProcessor

instance NFData StreamProcessor
