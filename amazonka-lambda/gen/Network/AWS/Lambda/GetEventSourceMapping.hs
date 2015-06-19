{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns configuration information for the specified event source mapping
-- (see CreateEventSourceMapping).
--
-- This operation requires permission for the
-- @lambda:GetEventSourceMapping@ action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_GetEventSourceMapping.html>
module Network.AWS.Lambda.GetEventSourceMapping
    (
    -- * Request
      GetEventSourceMapping
    -- ** Request constructor
    , getEventSourceMapping
    -- ** Request lenses
    , gesmUUID

    -- * Response
    , EventSourceMappingConfiguration
    -- ** Response constructor
    , eventSourceMappingConfiguration
    -- ** Response lenses
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUID
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesmUUID'
newtype GetEventSourceMapping = GetEventSourceMapping'{_gesmUUID :: Text} deriving (Eq, Read, Show)

-- | 'GetEventSourceMapping' smart constructor.
getEventSourceMapping :: Text -> GetEventSourceMapping
getEventSourceMapping pUUID = GetEventSourceMapping'{_gesmUUID = pUUID};

-- | The AWS Lambda assigned ID of the event source mapping.
gesmUUID :: Lens' GetEventSourceMapping Text
gesmUUID = lens _gesmUUID (\ s a -> s{_gesmUUID = a});

instance AWSRequest GetEventSourceMapping where
        type Sv GetEventSourceMapping = Lambda
        type Rs GetEventSourceMapping =
             EventSourceMappingConfiguration
        request = get
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders GetEventSourceMapping where
        toHeaders = const mempty

instance ToPath GetEventSourceMapping where
        toPath GetEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toText _gesmUUID]

instance ToQuery GetEventSourceMapping where
        toQuery = const mempty
