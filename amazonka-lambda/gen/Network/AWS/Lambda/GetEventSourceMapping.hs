{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration information for the specified event source mapping
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
    , gesmrqUUId

    -- * Response
    , EventSourceMappingConfiguration
    -- ** Response constructor
    , eventSourceMappingConfiguration
    -- ** Response lenses
    , gesmrsEventSourceARN
    , gesmrsFunctionARN
    , gesmrsState
    , gesmrsUUId
    , gesmrsLastProcessingResult
    , gesmrsBatchSize
    , gesmrsStateTransitionReason
    , gesmrsLastModified
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesmrqUUId'
newtype GetEventSourceMapping = GetEventSourceMapping'
    { _gesmrqUUId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetEventSourceMapping' smart constructor.
getEventSourceMapping :: Text -> GetEventSourceMapping
getEventSourceMapping pUUId =
    GetEventSourceMapping'
    { _gesmrqUUId = pUUId
    }

-- | The AWS Lambda assigned ID of the event source mapping.
gesmrqUUId :: Lens' GetEventSourceMapping Text
gesmrqUUId = lens _gesmrqUUId (\ s a -> s{_gesmrqUUId = a});

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
               toText _gesmrqUUId]

instance ToQuery GetEventSourceMapping where
        toQuery = const mempty
