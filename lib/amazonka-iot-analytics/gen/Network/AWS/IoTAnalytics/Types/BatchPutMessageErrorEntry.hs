{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.BatchPutMessageErrorEntry where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains informations about errors.
--
--
--
-- /See:/ 'batchPutMessageErrorEntry' smart constructor.
data BatchPutMessageErrorEntry = BatchPutMessageErrorEntry'
  { _bpmeeErrorCode ::
      !(Maybe Text),
    _bpmeeErrorMessage :: !(Maybe Text),
    _bpmeeMessageId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchPutMessageErrorEntry' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpmeeErrorCode' - The code associated with the error.
--
-- * 'bpmeeErrorMessage' - The message associated with the error.
--
-- * 'bpmeeMessageId' - The ID of the message that caused the error. See the value corresponding to the @messageId@ key in the message object.
batchPutMessageErrorEntry ::
  BatchPutMessageErrorEntry
batchPutMessageErrorEntry =
  BatchPutMessageErrorEntry'
    { _bpmeeErrorCode = Nothing,
      _bpmeeErrorMessage = Nothing,
      _bpmeeMessageId = Nothing
    }

-- | The code associated with the error.
bpmeeErrorCode :: Lens' BatchPutMessageErrorEntry (Maybe Text)
bpmeeErrorCode = lens _bpmeeErrorCode (\s a -> s {_bpmeeErrorCode = a})

-- | The message associated with the error.
bpmeeErrorMessage :: Lens' BatchPutMessageErrorEntry (Maybe Text)
bpmeeErrorMessage = lens _bpmeeErrorMessage (\s a -> s {_bpmeeErrorMessage = a})

-- | The ID of the message that caused the error. See the value corresponding to the @messageId@ key in the message object.
bpmeeMessageId :: Lens' BatchPutMessageErrorEntry (Maybe Text)
bpmeeMessageId = lens _bpmeeMessageId (\s a -> s {_bpmeeMessageId = a})

instance FromJSON BatchPutMessageErrorEntry where
  parseJSON =
    withObject
      "BatchPutMessageErrorEntry"
      ( \x ->
          BatchPutMessageErrorEntry'
            <$> (x .:? "errorCode")
            <*> (x .:? "errorMessage")
            <*> (x .:? "messageId")
      )

instance Hashable BatchPutMessageErrorEntry

instance NFData BatchPutMessageErrorEntry
