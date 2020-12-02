{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.DeadLetterConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A @DeadLetterConfig@ object that contains information about a dead-letter queue configuration.
--
--
--
-- /See:/ 'deadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig' {_dlcARN :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeadLetterConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlcARN' - The ARN of the SQS queue specified as the target for the dead-letter queue.
deadLetterConfig ::
  DeadLetterConfig
deadLetterConfig = DeadLetterConfig' {_dlcARN = Nothing}

-- | The ARN of the SQS queue specified as the target for the dead-letter queue.
dlcARN :: Lens' DeadLetterConfig (Maybe Text)
dlcARN = lens _dlcARN (\s a -> s {_dlcARN = a})

instance FromJSON DeadLetterConfig where
  parseJSON =
    withObject
      "DeadLetterConfig"
      (\x -> DeadLetterConfig' <$> (x .:? "Arn"))

instance Hashable DeadLetterConfig

instance NFData DeadLetterConfig

instance ToJSON DeadLetterConfig where
  toJSON DeadLetterConfig' {..} =
    object (catMaybes [("Arn" .=) <$> _dlcARN])
