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
-- Module      : Network.AWS.IoT.SetLoggingOptions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options.
--
--
module Network.AWS.IoT.SetLoggingOptions
    (
    -- * Creating a Request
      setLoggingOptions
    , SetLoggingOptions
    -- * Request Lenses
    , sloLoggingOptionsPayload

    -- * Destructuring the Response
    , setLoggingOptionsResponse
    , SetLoggingOptionsResponse
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the SetLoggingOptions operation.
--
--
--
-- /See:/ 'setLoggingOptions' smart constructor.
newtype SetLoggingOptions = SetLoggingOptions'
  { _sloLoggingOptionsPayload :: LoggingOptionsPayload
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoggingOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sloLoggingOptionsPayload' - The logging options payload.
setLoggingOptions
    :: LoggingOptionsPayload -- ^ 'sloLoggingOptionsPayload'
    -> SetLoggingOptions
setLoggingOptions pLoggingOptionsPayload_ =
  SetLoggingOptions' {_sloLoggingOptionsPayload = pLoggingOptionsPayload_}


-- | The logging options payload.
sloLoggingOptionsPayload :: Lens' SetLoggingOptions LoggingOptionsPayload
sloLoggingOptionsPayload = lens _sloLoggingOptionsPayload (\ s a -> s{_sloLoggingOptionsPayload = a})

instance AWSRequest SetLoggingOptions where
        type Rs SetLoggingOptions = SetLoggingOptionsResponse
        request = postJSON ioT
        response = receiveNull SetLoggingOptionsResponse'

instance Hashable SetLoggingOptions where

instance NFData SetLoggingOptions where

instance ToHeaders SetLoggingOptions where
        toHeaders = const mempty

instance ToJSON SetLoggingOptions where
        toJSON SetLoggingOptions'{..}
          = object
              (catMaybes
                 [Just
                    ("loggingOptionsPayload" .=
                       _sloLoggingOptionsPayload)])

instance ToPath SetLoggingOptions where
        toPath = const "/loggingOptions"

instance ToQuery SetLoggingOptions where
        toQuery = const mempty

-- | /See:/ 'setLoggingOptionsResponse' smart constructor.
data SetLoggingOptionsResponse =
  SetLoggingOptionsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetLoggingOptionsResponse' with the minimum fields required to make a request.
--
setLoggingOptionsResponse
    :: SetLoggingOptionsResponse
setLoggingOptionsResponse = SetLoggingOptionsResponse'


instance NFData SetLoggingOptionsResponse where
