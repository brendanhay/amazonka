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
-- Module      : Network.AWS.AppStream.StopFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified fleet.
--
--
module Network.AWS.AppStream.StopFleet
    (
    -- * Creating a Request
      stopFleet
    , StopFleet
    -- * Request Lenses
    , sfName

    -- * Destructuring the Response
    , stopFleetResponse
    , StopFleetResponse
    -- * Response Lenses
    , storsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopFleet' smart constructor.
newtype StopFleet = StopFleet'
  { _sfName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sfName' - The name of the fleet.
stopFleet
    :: Text -- ^ 'sfName'
    -> StopFleet
stopFleet pName_ = StopFleet' {_sfName = pName_}


-- | The name of the fleet.
sfName :: Lens' StopFleet Text
sfName = lens _sfName (\ s a -> s{_sfName = a})

instance AWSRequest StopFleet where
        type Rs StopFleet = StopFleetResponse
        request = postJSON appStream
        response
          = receiveEmpty
              (\ s h x ->
                 StopFleetResponse' <$> (pure (fromEnum s)))

instance Hashable StopFleet where

instance NFData StopFleet where

instance ToHeaders StopFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.StopFleet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopFleet where
        toJSON StopFleet'{..}
          = object (catMaybes [Just ("Name" .= _sfName)])

instance ToPath StopFleet where
        toPath = const "/"

instance ToQuery StopFleet where
        toQuery = const mempty

-- | /See:/ 'stopFleetResponse' smart constructor.
newtype StopFleetResponse = StopFleetResponse'
  { _storsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'storsResponseStatus' - -- | The response status code.
stopFleetResponse
    :: Int -- ^ 'storsResponseStatus'
    -> StopFleetResponse
stopFleetResponse pResponseStatus_ =
  StopFleetResponse' {_storsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
storsResponseStatus :: Lens' StopFleetResponse Int
storsResponseStatus = lens _storsResponseStatus (\ s a -> s{_storsResponseStatus = a})

instance NFData StopFleetResponse where
