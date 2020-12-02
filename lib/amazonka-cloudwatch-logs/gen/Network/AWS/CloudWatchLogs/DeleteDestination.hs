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
-- Module      : Network.AWS.CloudWatchLogs.DeleteDestination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified destination, and eventually disables all the subscription filters that publish to it. This operation does not delete the physical resource encapsulated by the destination.
--
--
module Network.AWS.CloudWatchLogs.DeleteDestination
    (
    -- * Creating a Request
      deleteDestination
    , DeleteDestination
    -- * Request Lenses
    , ddDestinationName

    -- * Destructuring the Response
    , deleteDestinationResponse
    , DeleteDestinationResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDestination' smart constructor.
newtype DeleteDestination = DeleteDestination'
  { _ddDestinationName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddDestinationName' - The name of the destination.
deleteDestination
    :: Text -- ^ 'ddDestinationName'
    -> DeleteDestination
deleteDestination pDestinationName_ =
  DeleteDestination' {_ddDestinationName = pDestinationName_}


-- | The name of the destination.
ddDestinationName :: Lens' DeleteDestination Text
ddDestinationName = lens _ddDestinationName (\ s a -> s{_ddDestinationName = a})

instance AWSRequest DeleteDestination where
        type Rs DeleteDestination = DeleteDestinationResponse
        request = postJSON cloudWatchLogs
        response = receiveNull DeleteDestinationResponse'

instance Hashable DeleteDestination where

instance NFData DeleteDestination where

instance ToHeaders DeleteDestination where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteDestination" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDestination where
        toJSON DeleteDestination'{..}
          = object
              (catMaybes
                 [Just ("destinationName" .= _ddDestinationName)])

instance ToPath DeleteDestination where
        toPath = const "/"

instance ToQuery DeleteDestination where
        toQuery = const mempty

-- | /See:/ 'deleteDestinationResponse' smart constructor.
data DeleteDestinationResponse =
  DeleteDestinationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDestinationResponse' with the minimum fields required to make a request.
--
deleteDestinationResponse
    :: DeleteDestinationResponse
deleteDestinationResponse = DeleteDestinationResponse'


instance NFData DeleteDestinationResponse where
