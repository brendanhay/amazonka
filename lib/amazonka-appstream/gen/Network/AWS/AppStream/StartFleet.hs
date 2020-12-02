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
-- Module      : Network.AWS.AppStream.StartFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified fleet.
--
--
module Network.AWS.AppStream.StartFleet
    (
    -- * Creating a Request
      startFleet
    , StartFleet
    -- * Request Lenses
    , staName

    -- * Destructuring the Response
    , startFleetResponse
    , StartFleetResponse
    -- * Response Lenses
    , sfrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startFleet' smart constructor.
newtype StartFleet = StartFleet'
  { _staName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'staName' - The name of the fleet.
startFleet
    :: Text -- ^ 'staName'
    -> StartFleet
startFleet pName_ = StartFleet' {_staName = pName_}


-- | The name of the fleet.
staName :: Lens' StartFleet Text
staName = lens _staName (\ s a -> s{_staName = a})

instance AWSRequest StartFleet where
        type Rs StartFleet = StartFleetResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 StartFleetResponse' <$> (pure (fromEnum s)))

instance Hashable StartFleet where

instance NFData StartFleet where

instance ToHeaders StartFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.StartFleet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StartFleet where
        toJSON StartFleet'{..}
          = object (catMaybes [Just ("Name" .= _staName)])

instance ToPath StartFleet where
        toPath = const "/"

instance ToQuery StartFleet where
        toQuery = const mempty

-- | /See:/ 'startFleetResponse' smart constructor.
newtype StartFleetResponse = StartFleetResponse'
  { _sfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfrsResponseStatus' - -- | The response status code.
startFleetResponse
    :: Int -- ^ 'sfrsResponseStatus'
    -> StartFleetResponse
startFleetResponse pResponseStatus_ =
  StartFleetResponse' {_sfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sfrsResponseStatus :: Lens' StartFleetResponse Int
sfrsResponseStatus = lens _sfrsResponseStatus (\ s a -> s{_sfrsResponseStatus = a})

instance NFData StartFleetResponse where
