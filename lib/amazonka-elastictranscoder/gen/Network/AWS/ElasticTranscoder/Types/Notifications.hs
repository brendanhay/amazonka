{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Notifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Notifications where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.
--
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--
-- /See:/ 'notifications' smart constructor.
data Notifications = Notifications'
  { _nError :: !(Maybe Text),
    _nWarning :: !(Maybe Text),
    _nProgressing :: !(Maybe Text),
    _nCompleted :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Notifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nError' - The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
-- * 'nWarning' - The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
--
-- * 'nProgressing' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
--
-- * 'nCompleted' - The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
notifications ::
  Notifications
notifications =
  Notifications'
    { _nError = Nothing,
      _nWarning = Nothing,
      _nProgressing = Nothing,
      _nCompleted = Nothing
    }

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
nError :: Lens' Notifications (Maybe Text)
nError = lens _nError (\s a -> s {_nError = a})

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
nWarning :: Lens' Notifications (Maybe Text)
nWarning = lens _nWarning (\s a -> s {_nWarning = a})

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
nProgressing :: Lens' Notifications (Maybe Text)
nProgressing = lens _nProgressing (\s a -> s {_nProgressing = a})

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
nCompleted :: Lens' Notifications (Maybe Text)
nCompleted = lens _nCompleted (\s a -> s {_nCompleted = a})

instance FromJSON Notifications where
  parseJSON =
    withObject
      "Notifications"
      ( \x ->
          Notifications'
            <$> (x .:? "Error")
            <*> (x .:? "Warning")
            <*> (x .:? "Progressing")
            <*> (x .:? "Completed")
      )

instance Hashable Notifications

instance NFData Notifications

instance ToJSON Notifications where
  toJSON Notifications' {..} =
    object
      ( catMaybes
          [ ("Error" .=) <$> _nError,
            ("Warning" .=) <$> _nWarning,
            ("Progressing" .=) <$> _nProgressing,
            ("Completed" .=) <$> _nCompleted
          ]
      )
