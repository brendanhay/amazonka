{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.DeleteEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes an event source mapping. This means AWS Lambda will no longer
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
    , desmrqUUId

    -- * Response
    , EventSourceMappingConfiguration
    -- ** Response constructor
    , eventSourceMappingConfiguration
    -- ** Response lenses
    , desmrsEventSourceARN
    , desmrsFunctionARN
    , desmrsState
    , desmrsUUId
    , desmrsLastProcessingResult
    , desmrsBatchSize
    , desmrsStateTransitionReason
    , desmrsLastModified
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desmrqUUId'
newtype DeleteEventSourceMapping = DeleteEventSourceMapping'
    { _desmrqUUId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteEventSourceMapping' smart constructor.
deleteEventSourceMapping :: Text -> DeleteEventSourceMapping
deleteEventSourceMapping pUUId =
    DeleteEventSourceMapping'
    { _desmrqUUId = pUUId
    }

-- | The event source mapping ID.
desmrqUUId :: Lens' DeleteEventSourceMapping Text
desmrqUUId = lens _desmrqUUId (\ s a -> s{_desmrqUUId = a});

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
               toText _desmrqUUId]

instance ToQuery DeleteEventSourceMapping where
        toQuery = const mempty
