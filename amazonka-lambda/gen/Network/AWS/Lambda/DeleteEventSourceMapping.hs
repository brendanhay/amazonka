{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Lambda.DeleteEventSourceMapping
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

-- | Removes an event source mapping. This means AWS Lambda will no longer
-- invoke the function for events in the associated source.
--
-- This operation requires permission for the
-- @lambda:DeleteEventSourceMapping@ action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_DeleteEventSourceMapping.html>
module Network.AWS.Lambda.DeleteEventSourceMapping
    (
    -- * Request
      DeleteEventSourceMapping
    -- ** Request constructor
    , deleteEventSourceMapping
    -- ** Request lenses
    , desmUUID

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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Lambda.Types

-- | /See:/ 'deleteEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmUUID'
newtype DeleteEventSourceMapping = DeleteEventSourceMapping'{_desmUUID :: Text} deriving (Eq, Read, Show)

-- | 'DeleteEventSourceMapping' smart constructor.
deleteEventSourceMapping :: Text -> DeleteEventSourceMapping
deleteEventSourceMapping pUUID = DeleteEventSourceMapping'{_desmUUID = pUUID};

-- | The event source mapping ID.
desmUUID :: Lens' DeleteEventSourceMapping Text
desmUUID = lens _desmUUID (\ s a -> s{_desmUUID = a});

instance AWSRequest DeleteEventSourceMapping where
        type Sv DeleteEventSourceMapping = Lambda
        type Rs DeleteEventSourceMapping =
             EventSourceMappingConfiguration
        request = delete
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders DeleteEventSourceMapping where
        toHeaders = const mempty

instance ToPath DeleteEventSourceMapping where
        toPath DeleteEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toText _desmUUID]

instance ToQuery DeleteEventSourceMapping where
        toQuery = const mempty
