{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
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
    , desmUUId

    -- * Response
    , EventSourceMappingConfiguration
    -- ** Response constructor
    , eventSourceMappingConfiguration
    -- ** Response lenses
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmUUId'
newtype DeleteEventSourceMapping = DeleteEventSourceMapping'
    { _desmUUId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEventSourceMapping' smart constructor.
deleteEventSourceMapping :: Text -> DeleteEventSourceMapping
deleteEventSourceMapping pUUId =
    DeleteEventSourceMapping'
    { _desmUUId = pUUId
    }

-- | The event source mapping ID.
desmUUId :: Lens' DeleteEventSourceMapping Text
desmUUId = lens _desmUUId (\ s a -> s{_desmUUId = a});

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
               toText _desmUUId]

instance ToQuery DeleteEventSourceMapping where
        toQuery = const mempty
