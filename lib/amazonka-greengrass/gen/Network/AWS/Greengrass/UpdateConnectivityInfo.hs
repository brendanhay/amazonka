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
-- Module      : Network.AWS.Greengrass.UpdateConnectivityInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the connectivity information for the core. Any devices that belong to the group which has this core will receive this information in order to find the location of the core and connect to it.
module Network.AWS.Greengrass.UpdateConnectivityInfo
    (
    -- * Creating a Request
      updateConnectivityInfo
    , UpdateConnectivityInfo
    -- * Request Lenses
    , uciConnectivityInfo
    , uciThingName

    -- * Destructuring the Response
    , updateConnectivityInfoResponse
    , UpdateConnectivityInfoResponse
    -- * Response Lenses
    , ucirsVersion
    , ucirsMessage
    , ucirsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Connectivity information.
--
-- /See:/ 'updateConnectivityInfo' smart constructor.
data UpdateConnectivityInfo = UpdateConnectivityInfo'
  { _uciConnectivityInfo :: !(Maybe [ConnectivityInfo])
  , _uciThingName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConnectivityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uciConnectivityInfo' - A list of connectivity info.
--
-- * 'uciThingName' - The thing name.
updateConnectivityInfo
    :: Text -- ^ 'uciThingName'
    -> UpdateConnectivityInfo
updateConnectivityInfo pThingName_ =
  UpdateConnectivityInfo'
    {_uciConnectivityInfo = Nothing, _uciThingName = pThingName_}


-- | A list of connectivity info.
uciConnectivityInfo :: Lens' UpdateConnectivityInfo [ConnectivityInfo]
uciConnectivityInfo = lens _uciConnectivityInfo (\ s a -> s{_uciConnectivityInfo = a}) . _Default . _Coerce

-- | The thing name.
uciThingName :: Lens' UpdateConnectivityInfo Text
uciThingName = lens _uciThingName (\ s a -> s{_uciThingName = a})

instance AWSRequest UpdateConnectivityInfo where
        type Rs UpdateConnectivityInfo =
             UpdateConnectivityInfoResponse
        request = putJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 UpdateConnectivityInfoResponse' <$>
                   (x .?> "Version") <*> (x .?> "message") <*>
                     (pure (fromEnum s)))

instance Hashable UpdateConnectivityInfo where

instance NFData UpdateConnectivityInfo where

instance ToHeaders UpdateConnectivityInfo where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateConnectivityInfo where
        toJSON UpdateConnectivityInfo'{..}
          = object
              (catMaybes
                 [("ConnectivityInfo" .=) <$> _uciConnectivityInfo])

instance ToPath UpdateConnectivityInfo where
        toPath UpdateConnectivityInfo'{..}
          = mconcat
              ["/greengrass/things/", toBS _uciThingName,
               "/connectivityInfo"]

instance ToQuery UpdateConnectivityInfo where
        toQuery = const mempty

-- | /See:/ 'updateConnectivityInfoResponse' smart constructor.
data UpdateConnectivityInfoResponse = UpdateConnectivityInfoResponse'
  { _ucirsVersion        :: !(Maybe Text)
  , _ucirsMessage        :: !(Maybe Text)
  , _ucirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConnectivityInfoResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucirsVersion' - The new version of the connectivity info.
--
-- * 'ucirsMessage' - A message about the connectivity info update request.
--
-- * 'ucirsResponseStatus' - -- | The response status code.
updateConnectivityInfoResponse
    :: Int -- ^ 'ucirsResponseStatus'
    -> UpdateConnectivityInfoResponse
updateConnectivityInfoResponse pResponseStatus_ =
  UpdateConnectivityInfoResponse'
    { _ucirsVersion = Nothing
    , _ucirsMessage = Nothing
    , _ucirsResponseStatus = pResponseStatus_
    }


-- | The new version of the connectivity info.
ucirsVersion :: Lens' UpdateConnectivityInfoResponse (Maybe Text)
ucirsVersion = lens _ucirsVersion (\ s a -> s{_ucirsVersion = a})

-- | A message about the connectivity info update request.
ucirsMessage :: Lens' UpdateConnectivityInfoResponse (Maybe Text)
ucirsMessage = lens _ucirsMessage (\ s a -> s{_ucirsMessage = a})

-- | -- | The response status code.
ucirsResponseStatus :: Lens' UpdateConnectivityInfoResponse Int
ucirsResponseStatus = lens _ucirsResponseStatus (\ s a -> s{_ucirsResponseStatus = a})

instance NFData UpdateConnectivityInfoResponse where
