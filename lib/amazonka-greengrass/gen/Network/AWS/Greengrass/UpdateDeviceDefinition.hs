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
-- Module      : Network.AWS.Greengrass.UpdateDeviceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a device definition.
module Network.AWS.Greengrass.UpdateDeviceDefinition
    (
    -- * Creating a Request
      updateDeviceDefinition
    , UpdateDeviceDefinition
    -- * Request Lenses
    , uddName
    , uddDeviceDefinitionId

    -- * Destructuring the Response
    , updateDeviceDefinitionResponse
    , UpdateDeviceDefinitionResponse
    -- * Response Lenses
    , uddrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDeviceDefinition' smart constructor.
data UpdateDeviceDefinition = UpdateDeviceDefinition'
  { _uddName               :: !(Maybe Text)
  , _uddDeviceDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeviceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uddName' - The name of the definition.
--
-- * 'uddDeviceDefinitionId' - The ID of the device definition.
updateDeviceDefinition
    :: Text -- ^ 'uddDeviceDefinitionId'
    -> UpdateDeviceDefinition
updateDeviceDefinition pDeviceDefinitionId_ =
  UpdateDeviceDefinition'
    {_uddName = Nothing, _uddDeviceDefinitionId = pDeviceDefinitionId_}


-- | The name of the definition.
uddName :: Lens' UpdateDeviceDefinition (Maybe Text)
uddName = lens _uddName (\ s a -> s{_uddName = a})

-- | The ID of the device definition.
uddDeviceDefinitionId :: Lens' UpdateDeviceDefinition Text
uddDeviceDefinitionId = lens _uddDeviceDefinitionId (\ s a -> s{_uddDeviceDefinitionId = a})

instance AWSRequest UpdateDeviceDefinition where
        type Rs UpdateDeviceDefinition =
             UpdateDeviceDefinitionResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDeviceDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateDeviceDefinition where

instance NFData UpdateDeviceDefinition where

instance ToHeaders UpdateDeviceDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDeviceDefinition where
        toJSON UpdateDeviceDefinition'{..}
          = object (catMaybes [("Name" .=) <$> _uddName])

instance ToPath UpdateDeviceDefinition where
        toPath UpdateDeviceDefinition'{..}
          = mconcat
              ["/greengrass/definition/devices/",
               toBS _uddDeviceDefinitionId]

instance ToQuery UpdateDeviceDefinition where
        toQuery = const mempty

-- | /See:/ 'updateDeviceDefinitionResponse' smart constructor.
newtype UpdateDeviceDefinitionResponse = UpdateDeviceDefinitionResponse'
  { _uddrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeviceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uddrsResponseStatus' - -- | The response status code.
updateDeviceDefinitionResponse
    :: Int -- ^ 'uddrsResponseStatus'
    -> UpdateDeviceDefinitionResponse
updateDeviceDefinitionResponse pResponseStatus_ =
  UpdateDeviceDefinitionResponse' {_uddrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uddrsResponseStatus :: Lens' UpdateDeviceDefinitionResponse Int
uddrsResponseStatus = lens _uddrsResponseStatus (\ s a -> s{_uddrsResponseStatus = a})

instance NFData UpdateDeviceDefinitionResponse where
