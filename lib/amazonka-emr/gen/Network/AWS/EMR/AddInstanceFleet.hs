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
-- Module      : Network.AWS.EMR.AddInstanceFleet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an instance fleet to a running cluster.
--
--
module Network.AWS.EMR.AddInstanceFleet
    (
    -- * Creating a Request
      addInstanceFleet
    , AddInstanceFleet
    -- * Request Lenses
    , aifClusterId
    , aifInstanceFleet

    -- * Destructuring the Response
    , addInstanceFleetResponse
    , AddInstanceFleetResponse
    -- * Response Lenses
    , aifrsClusterId
    , aifrsInstanceFleetId
    , aifrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addInstanceFleet' smart constructor.
data AddInstanceFleet = AddInstanceFleet'
  { _aifClusterId     :: !Text
  , _aifInstanceFleet :: !InstanceFleetConfig
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddInstanceFleet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aifClusterId' - The unique identifier of the cluster.
--
-- * 'aifInstanceFleet' - Specifies the configuration of the instance fleet.
addInstanceFleet
    :: Text -- ^ 'aifClusterId'
    -> InstanceFleetConfig -- ^ 'aifInstanceFleet'
    -> AddInstanceFleet
addInstanceFleet pClusterId_ pInstanceFleet_ =
  AddInstanceFleet'
    {_aifClusterId = pClusterId_, _aifInstanceFleet = pInstanceFleet_}


-- | The unique identifier of the cluster.
aifClusterId :: Lens' AddInstanceFleet Text
aifClusterId = lens _aifClusterId (\ s a -> s{_aifClusterId = a})

-- | Specifies the configuration of the instance fleet.
aifInstanceFleet :: Lens' AddInstanceFleet InstanceFleetConfig
aifInstanceFleet = lens _aifInstanceFleet (\ s a -> s{_aifInstanceFleet = a})

instance AWSRequest AddInstanceFleet where
        type Rs AddInstanceFleet = AddInstanceFleetResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 AddInstanceFleetResponse' <$>
                   (x .?> "ClusterId") <*> (x .?> "InstanceFleetId") <*>
                     (pure (fromEnum s)))

instance Hashable AddInstanceFleet where

instance NFData AddInstanceFleet where

instance ToHeaders AddInstanceFleet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.AddInstanceFleet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddInstanceFleet where
        toJSON AddInstanceFleet'{..}
          = object
              (catMaybes
                 [Just ("ClusterId" .= _aifClusterId),
                  Just ("InstanceFleet" .= _aifInstanceFleet)])

instance ToPath AddInstanceFleet where
        toPath = const "/"

instance ToQuery AddInstanceFleet where
        toQuery = const mempty

-- | /See:/ 'addInstanceFleetResponse' smart constructor.
data AddInstanceFleetResponse = AddInstanceFleetResponse'
  { _aifrsClusterId       :: !(Maybe Text)
  , _aifrsInstanceFleetId :: !(Maybe Text)
  , _aifrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddInstanceFleetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aifrsClusterId' - The unique identifier of the cluster.
--
-- * 'aifrsInstanceFleetId' - The unique identifier of the instance fleet.
--
-- * 'aifrsResponseStatus' - -- | The response status code.
addInstanceFleetResponse
    :: Int -- ^ 'aifrsResponseStatus'
    -> AddInstanceFleetResponse
addInstanceFleetResponse pResponseStatus_ =
  AddInstanceFleetResponse'
    { _aifrsClusterId = Nothing
    , _aifrsInstanceFleetId = Nothing
    , _aifrsResponseStatus = pResponseStatus_
    }


-- | The unique identifier of the cluster.
aifrsClusterId :: Lens' AddInstanceFleetResponse (Maybe Text)
aifrsClusterId = lens _aifrsClusterId (\ s a -> s{_aifrsClusterId = a})

-- | The unique identifier of the instance fleet.
aifrsInstanceFleetId :: Lens' AddInstanceFleetResponse (Maybe Text)
aifrsInstanceFleetId = lens _aifrsInstanceFleetId (\ s a -> s{_aifrsInstanceFleetId = a})

-- | -- | The response status code.
aifrsResponseStatus :: Lens' AddInstanceFleetResponse Int
aifrsResponseStatus = lens _aifrsResponseStatus (\ s a -> s{_aifrsResponseStatus = a})

instance NFData AddInstanceFleetResponse where
