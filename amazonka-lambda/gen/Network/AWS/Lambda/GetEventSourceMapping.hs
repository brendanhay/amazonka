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
    , gesmUUId

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

-- | /See:/ 'getEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesmUUId'
newtype GetEventSourceMapping = GetEventSourceMapping'
    { _gesmUUId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetEventSourceMapping' smart constructor.
getEventSourceMapping :: Text -> GetEventSourceMapping
getEventSourceMapping pUUId_ =
    GetEventSourceMapping'
    { _gesmUUId = pUUId_
    }

-- | The AWS Lambda assigned ID of the event source mapping.
gesmUUId :: Lens' GetEventSourceMapping Text
gesmUUId = lens _gesmUUId (\ s a -> s{_gesmUUId = a});

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
               toText _gesmUUId]

instance ToQuery GetEventSourceMapping where
        toQuery = const mempty
