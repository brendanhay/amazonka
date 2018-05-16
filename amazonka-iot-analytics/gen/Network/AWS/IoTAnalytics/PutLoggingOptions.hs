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
-- Module      : Network.AWS.IoTAnalytics.PutLoggingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or updates the AWS IoT Analytics logging options.
--
--
module Network.AWS.IoTAnalytics.PutLoggingOptions
    (
    -- * Creating a Request
      putLoggingOptions
    , PutLoggingOptions
    -- * Request Lenses
    , ploLoggingOptions

    -- * Destructuring the Response
    , putLoggingOptionsResponse
    , PutLoggingOptionsResponse
    ) where

import Network.AWS.IoTAnalytics.Types
import Network.AWS.IoTAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putLoggingOptions' smart constructor.
newtype PutLoggingOptions = PutLoggingOptions'
  { _ploLoggingOptions :: LoggingOptions
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ploLoggingOptions' - The new values of the AWS IoT Analytics logging options.
putLoggingOptions
    :: LoggingOptions -- ^ 'ploLoggingOptions'
    -> PutLoggingOptions
putLoggingOptions pLoggingOptions_ =
  PutLoggingOptions' {_ploLoggingOptions = pLoggingOptions_}


-- | The new values of the AWS IoT Analytics logging options.
ploLoggingOptions :: Lens' PutLoggingOptions LoggingOptions
ploLoggingOptions = lens _ploLoggingOptions (\ s a -> s{_ploLoggingOptions = a})

instance AWSRequest PutLoggingOptions where
        type Rs PutLoggingOptions = PutLoggingOptionsResponse
        request = putJSON ioTAnalytics
        response = receiveNull PutLoggingOptionsResponse'

instance Hashable PutLoggingOptions where

instance NFData PutLoggingOptions where

instance ToHeaders PutLoggingOptions where
        toHeaders = const mempty

instance ToJSON PutLoggingOptions where
        toJSON PutLoggingOptions'{..}
          = object
              (catMaybes
                 [Just ("loggingOptions" .= _ploLoggingOptions)])

instance ToPath PutLoggingOptions where
        toPath = const "/logging"

instance ToQuery PutLoggingOptions where
        toQuery = const mempty

-- | /See:/ 'putLoggingOptionsResponse' smart constructor.
data PutLoggingOptionsResponse =
  PutLoggingOptionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutLoggingOptionsResponse' with the minimum fields required to make a request.
--
putLoggingOptionsResponse
    :: PutLoggingOptionsResponse
putLoggingOptionsResponse = PutLoggingOptionsResponse'


instance NFData PutLoggingOptionsResponse where
