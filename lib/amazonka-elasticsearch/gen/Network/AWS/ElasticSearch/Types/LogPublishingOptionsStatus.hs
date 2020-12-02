{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.LogPublishingOptionsStatus where

import Network.AWS.ElasticSearch.Types.LogPublishingOption
import Network.AWS.ElasticSearch.Types.LogType
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The configured log publishing options for the domain and their current status.
--
--
--
-- /See:/ 'logPublishingOptionsStatus' smart constructor.
data LogPublishingOptionsStatus = LogPublishingOptionsStatus'
  { _lposStatus ::
      !(Maybe OptionStatus),
    _lposOptions ::
      !( Maybe
           ( Map
               LogType
               (LogPublishingOption)
           )
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LogPublishingOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lposStatus' - The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
--
-- * 'lposOptions' - The log publishing options configured for the Elasticsearch domain.
logPublishingOptionsStatus ::
  LogPublishingOptionsStatus
logPublishingOptionsStatus =
  LogPublishingOptionsStatus'
    { _lposStatus = Nothing,
      _lposOptions = Nothing
    }

-- | The status of the log publishing options for the Elasticsearch domain. See @OptionStatus@ for the status information that's included.
lposStatus :: Lens' LogPublishingOptionsStatus (Maybe OptionStatus)
lposStatus = lens _lposStatus (\s a -> s {_lposStatus = a})

-- | The log publishing options configured for the Elasticsearch domain.
lposOptions :: Lens' LogPublishingOptionsStatus (HashMap LogType (LogPublishingOption))
lposOptions = lens _lposOptions (\s a -> s {_lposOptions = a}) . _Default . _Map

instance FromJSON LogPublishingOptionsStatus where
  parseJSON =
    withObject
      "LogPublishingOptionsStatus"
      ( \x ->
          LogPublishingOptionsStatus'
            <$> (x .:? "Status") <*> (x .:? "Options" .!= mempty)
      )

instance Hashable LogPublishingOptionsStatus

instance NFData LogPublishingOptionsStatus
