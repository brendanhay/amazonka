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
-- Module      : Network.AWS.Greengrass.DeleteConnectorDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a connector definition.
module Network.AWS.Greengrass.DeleteConnectorDefinition
    (
    -- * Creating a Request
      deleteConnectorDefinition
    , DeleteConnectorDefinition
    -- * Request Lenses
    , dcdConnectorDefinitionId

    -- * Destructuring the Response
    , deleteConnectorDefinitionResponse
    , DeleteConnectorDefinitionResponse
    -- * Response Lenses
    , dcdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConnectorDefinition' smart constructor.
newtype DeleteConnectorDefinition = DeleteConnectorDefinition'
  { _dcdConnectorDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConnectorDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdConnectorDefinitionId' - The ID of the connector definition.
deleteConnectorDefinition
    :: Text -- ^ 'dcdConnectorDefinitionId'
    -> DeleteConnectorDefinition
deleteConnectorDefinition pConnectorDefinitionId_ =
  DeleteConnectorDefinition'
    {_dcdConnectorDefinitionId = pConnectorDefinitionId_}


-- | The ID of the connector definition.
dcdConnectorDefinitionId :: Lens' DeleteConnectorDefinition Text
dcdConnectorDefinitionId = lens _dcdConnectorDefinitionId (\ s a -> s{_dcdConnectorDefinitionId = a})

instance AWSRequest DeleteConnectorDefinition where
        type Rs DeleteConnectorDefinition =
             DeleteConnectorDefinitionResponse
        request = delete greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteConnectorDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteConnectorDefinition where

instance NFData DeleteConnectorDefinition where

instance ToHeaders DeleteConnectorDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteConnectorDefinition where
        toPath DeleteConnectorDefinition'{..}
          = mconcat
              ["/greengrass/definition/connectors/",
               toBS _dcdConnectorDefinitionId]

instance ToQuery DeleteConnectorDefinition where
        toQuery = const mempty

-- | /See:/ 'deleteConnectorDefinitionResponse' smart constructor.
newtype DeleteConnectorDefinitionResponse = DeleteConnectorDefinitionResponse'
  { _dcdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteConnectorDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdrsResponseStatus' - -- | The response status code.
deleteConnectorDefinitionResponse
    :: Int -- ^ 'dcdrsResponseStatus'
    -> DeleteConnectorDefinitionResponse
deleteConnectorDefinitionResponse pResponseStatus_ =
  DeleteConnectorDefinitionResponse' {_dcdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcdrsResponseStatus :: Lens' DeleteConnectorDefinitionResponse Int
dcdrsResponseStatus = lens _dcdrsResponseStatus (\ s a -> s{_dcdrsResponseStatus = a})

instance NFData DeleteConnectorDefinitionResponse
         where
