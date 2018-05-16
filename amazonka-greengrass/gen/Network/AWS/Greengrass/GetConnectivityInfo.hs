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
-- Module      : Network.AWS.Greengrass.GetConnectivityInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the connectivity information for a core.
module Network.AWS.Greengrass.GetConnectivityInfo
    (
    -- * Creating a Request
      getConnectivityInfo
    , GetConnectivityInfo
    -- * Request Lenses
    , gciThingName

    -- * Destructuring the Response
    , getConnectivityInfoResponse
    , GetConnectivityInfoResponse
    -- * Response Lenses
    , gcirsMessage
    , gcirsConnectivityInfo
    , gcirsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getConnectivityInfo' smart constructor.
newtype GetConnectivityInfo = GetConnectivityInfo'
  { _gciThingName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConnectivityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gciThingName' - The thing name.
getConnectivityInfo
    :: Text -- ^ 'gciThingName'
    -> GetConnectivityInfo
getConnectivityInfo pThingName_ =
  GetConnectivityInfo' {_gciThingName = pThingName_}


-- | The thing name.
gciThingName :: Lens' GetConnectivityInfo Text
gciThingName = lens _gciThingName (\ s a -> s{_gciThingName = a})

instance AWSRequest GetConnectivityInfo where
        type Rs GetConnectivityInfo =
             GetConnectivityInfoResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetConnectivityInfoResponse' <$>
                   (x .?> "message") <*>
                     (x .?> "ConnectivityInfo" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetConnectivityInfo where

instance NFData GetConnectivityInfo where

instance ToHeaders GetConnectivityInfo where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetConnectivityInfo where
        toPath GetConnectivityInfo'{..}
          = mconcat
              ["/greengrass/things/", toBS _gciThingName,
               "/connectivityInfo"]

instance ToQuery GetConnectivityInfo where
        toQuery = const mempty

-- | /See:/ 'getConnectivityInfoResponse' smart constructor.
data GetConnectivityInfoResponse = GetConnectivityInfoResponse'
  { _gcirsMessage          :: !(Maybe Text)
  , _gcirsConnectivityInfo :: !(Maybe [ConnectivityInfo])
  , _gcirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConnectivityInfoResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirsMessage' - A message about the connectivity info request.
--
-- * 'gcirsConnectivityInfo' - Connectivity info list.
--
-- * 'gcirsResponseStatus' - -- | The response status code.
getConnectivityInfoResponse
    :: Int -- ^ 'gcirsResponseStatus'
    -> GetConnectivityInfoResponse
getConnectivityInfoResponse pResponseStatus_ =
  GetConnectivityInfoResponse'
    { _gcirsMessage = Nothing
    , _gcirsConnectivityInfo = Nothing
    , _gcirsResponseStatus = pResponseStatus_
    }


-- | A message about the connectivity info request.
gcirsMessage :: Lens' GetConnectivityInfoResponse (Maybe Text)
gcirsMessage = lens _gcirsMessage (\ s a -> s{_gcirsMessage = a})

-- | Connectivity info list.
gcirsConnectivityInfo :: Lens' GetConnectivityInfoResponse [ConnectivityInfo]
gcirsConnectivityInfo = lens _gcirsConnectivityInfo (\ s a -> s{_gcirsConnectivityInfo = a}) . _Default . _Coerce

-- | -- | The response status code.
gcirsResponseStatus :: Lens' GetConnectivityInfoResponse Int
gcirsResponseStatus = lens _gcirsResponseStatus (\ s a -> s{_gcirsResponseStatus = a})

instance NFData GetConnectivityInfoResponse where
