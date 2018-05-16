{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetV2LoggingLevel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging level.
--
--
module Network.AWS.IoT.SetV2LoggingLevel
    (
    -- * Creating a Request
      setV2LoggingLevel
    , SetV2LoggingLevel
    -- * Request Lenses
    , svllLogTarget
    , svllLogLevel

    -- * Destructuring the Response
    , setV2LoggingLevelResponse
    , SetV2LoggingLevelResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setV2LoggingLevel' smart constructor.
data SetV2LoggingLevel = SetV2LoggingLevel'
  { _svllLogTarget :: !LogTarget
  , _svllLogLevel  :: !LogLevel
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetV2LoggingLevel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'svllLogTarget' - The log target.
--
-- * 'svllLogLevel' - The log level.
setV2LoggingLevel
    :: LogTarget -- ^ 'svllLogTarget'
    -> LogLevel -- ^ 'svllLogLevel'
    -> SetV2LoggingLevel
setV2LoggingLevel pLogTarget_ pLogLevel_ =
  SetV2LoggingLevel' {_svllLogTarget = pLogTarget_, _svllLogLevel = pLogLevel_}


-- | The log target.
svllLogTarget :: Lens' SetV2LoggingLevel LogTarget
svllLogTarget = lens _svllLogTarget (\ s a -> s{_svllLogTarget = a})

-- | The log level.
svllLogLevel :: Lens' SetV2LoggingLevel LogLevel
svllLogLevel = lens _svllLogLevel (\ s a -> s{_svllLogLevel = a})

instance AWSRequest SetV2LoggingLevel where
        type Rs SetV2LoggingLevel = SetV2LoggingLevelResponse
        request = postJSON ioT
        response = receiveNull SetV2LoggingLevelResponse'

instance Hashable SetV2LoggingLevel where

instance NFData SetV2LoggingLevel where

instance ToHeaders SetV2LoggingLevel where
        toHeaders = const mempty

instance ToJSON SetV2LoggingLevel where
        toJSON SetV2LoggingLevel'{..}
          = object
              (catMaybes
                 [Just ("logTarget" .= _svllLogTarget),
                  Just ("logLevel" .= _svllLogLevel)])

instance ToPath SetV2LoggingLevel where
        toPath = const "/v2LoggingLevel"

instance ToQuery SetV2LoggingLevel where
        toQuery = const mempty

-- | /See:/ 'setV2LoggingLevelResponse' smart constructor.
data SetV2LoggingLevelResponse =
  SetV2LoggingLevelResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetV2LoggingLevelResponse' with the minimum fields required to make a request.
--
setV2LoggingLevelResponse
    :: SetV2LoggingLevelResponse
setV2LoggingLevelResponse = SetV2LoggingLevelResponse'


instance NFData SetV2LoggingLevelResponse where
