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
-- Module      : Network.AWS.Lambda.DeleteEventSourceMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an event source mapping. This means AWS Lambda will no longer invoke the function for events in the associated source.
--
--
-- This operation requires permission for the @lambda:DeleteEventSourceMapping@ action.
--
module Network.AWS.Lambda.DeleteEventSourceMapping
    (
    -- * Creating a Request
      deleteEventSourceMapping
    , DeleteEventSourceMapping
    -- * Request Lenses
    , desmUUId

    -- * Destructuring the Response
    , eventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    -- * Response Lenses
    , esmcEventSourceARN
    , esmcState
    , esmcFunctionARN
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteEventSourceMapping' smart constructor.
newtype DeleteEventSourceMapping = DeleteEventSourceMapping'
  { _desmUUId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desmUUId' - The event source mapping ID.
deleteEventSourceMapping
    :: Text -- ^ 'desmUUId'
    -> DeleteEventSourceMapping
deleteEventSourceMapping pUUId_ = DeleteEventSourceMapping' {_desmUUId = pUUId_}


-- | The event source mapping ID.
desmUUId :: Lens' DeleteEventSourceMapping Text
desmUUId = lens _desmUUId (\ s a -> s{_desmUUId = a})

instance AWSRequest DeleteEventSourceMapping where
        type Rs DeleteEventSourceMapping =
             EventSourceMappingConfiguration
        request = delete lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable DeleteEventSourceMapping where

instance NFData DeleteEventSourceMapping where

instance ToHeaders DeleteEventSourceMapping where
        toHeaders = const mempty

instance ToPath DeleteEventSourceMapping where
        toPath DeleteEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toBS _desmUUId]

instance ToQuery DeleteEventSourceMapping where
        toQuery = const mempty
