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
-- Module      : Network.AWS.Greengrass.UpdateConnectorDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a connector definition.
module Network.AWS.Greengrass.UpdateConnectorDefinition
    (
    -- * Creating a Request
      updateConnectorDefinition
    , UpdateConnectorDefinition
    -- * Request Lenses
    , uName
    , uConnectorDefinitionId

    -- * Destructuring the Response
    , updateConnectorDefinitionResponse
    , UpdateConnectorDefinitionResponse
    -- * Response Lenses
    , ucdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateConnectorDefinition' smart constructor.
data UpdateConnectorDefinition = UpdateConnectorDefinition'
  { _uName                  :: !(Maybe Text)
  , _uConnectorDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConnectorDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uName' - The name of the definition.
--
-- * 'uConnectorDefinitionId' - The ID of the connector definition.
updateConnectorDefinition
    :: Text -- ^ 'uConnectorDefinitionId'
    -> UpdateConnectorDefinition
updateConnectorDefinition pConnectorDefinitionId_ =
  UpdateConnectorDefinition'
    {_uName = Nothing, _uConnectorDefinitionId = pConnectorDefinitionId_}


-- | The name of the definition.
uName :: Lens' UpdateConnectorDefinition (Maybe Text)
uName = lens _uName (\ s a -> s{_uName = a})

-- | The ID of the connector definition.
uConnectorDefinitionId :: Lens' UpdateConnectorDefinition Text
uConnectorDefinitionId = lens _uConnectorDefinitionId (\ s a -> s{_uConnectorDefinitionId = a})

instance AWSRequest UpdateConnectorDefinition where
        type Rs UpdateConnectorDefinition =
             UpdateConnectorDefinitionResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateConnectorDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateConnectorDefinition where

instance NFData UpdateConnectorDefinition where

instance ToHeaders UpdateConnectorDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateConnectorDefinition where
        toJSON UpdateConnectorDefinition'{..}
          = object (catMaybes [("Name" .=) <$> _uName])

instance ToPath UpdateConnectorDefinition where
        toPath UpdateConnectorDefinition'{..}
          = mconcat
              ["/greengrass/definition/connectors/",
               toBS _uConnectorDefinitionId]

instance ToQuery UpdateConnectorDefinition where
        toQuery = const mempty

-- | /See:/ 'updateConnectorDefinitionResponse' smart constructor.
newtype UpdateConnectorDefinitionResponse = UpdateConnectorDefinitionResponse'
  { _ucdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucdrsResponseStatus' - -- | The response status code.
updateConnectorDefinitionResponse
    :: Int -- ^ 'ucdrsResponseStatus'
    -> UpdateConnectorDefinitionResponse
updateConnectorDefinitionResponse pResponseStatus_ =
  UpdateConnectorDefinitionResponse' {_ucdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucdrsResponseStatus :: Lens' UpdateConnectorDefinitionResponse Int
ucdrsResponseStatus = lens _ucdrsResponseStatus (\ s a -> s{_ucdrsResponseStatus = a})

instance NFData UpdateConnectorDefinitionResponse
         where
