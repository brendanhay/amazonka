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
-- Module      : Network.AWS.Greengrass.UpdateResourceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a resource definition.
module Network.AWS.Greengrass.UpdateResourceDefinition
    (
    -- * Creating a Request
      updateResourceDefinition
    , UpdateResourceDefinition
    -- * Request Lenses
    , urdName
    , urdResourceDefinitionId

    -- * Destructuring the Response
    , updateResourceDefinitionResponse
    , UpdateResourceDefinitionResponse
    -- * Response Lenses
    , urdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateResourceDefinition' smart constructor.
data UpdateResourceDefinition = UpdateResourceDefinition'
  { _urdName                 :: !(Maybe Text)
  , _urdResourceDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResourceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdName' - The name of the definition.
--
-- * 'urdResourceDefinitionId' - The ID of the resource definition.
updateResourceDefinition
    :: Text -- ^ 'urdResourceDefinitionId'
    -> UpdateResourceDefinition
updateResourceDefinition pResourceDefinitionId_ =
  UpdateResourceDefinition'
    {_urdName = Nothing, _urdResourceDefinitionId = pResourceDefinitionId_}


-- | The name of the definition.
urdName :: Lens' UpdateResourceDefinition (Maybe Text)
urdName = lens _urdName (\ s a -> s{_urdName = a})

-- | The ID of the resource definition.
urdResourceDefinitionId :: Lens' UpdateResourceDefinition Text
urdResourceDefinitionId = lens _urdResourceDefinitionId (\ s a -> s{_urdResourceDefinitionId = a})

instance AWSRequest UpdateResourceDefinition where
        type Rs UpdateResourceDefinition =
             UpdateResourceDefinitionResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateResourceDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateResourceDefinition where

instance NFData UpdateResourceDefinition where

instance ToHeaders UpdateResourceDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateResourceDefinition where
        toJSON UpdateResourceDefinition'{..}
          = object (catMaybes [("Name" .=) <$> _urdName])

instance ToPath UpdateResourceDefinition where
        toPath UpdateResourceDefinition'{..}
          = mconcat
              ["/greengrass/definition/resources/",
               toBS _urdResourceDefinitionId]

instance ToQuery UpdateResourceDefinition where
        toQuery = const mempty

-- | /See:/ 'updateResourceDefinitionResponse' smart constructor.
newtype UpdateResourceDefinitionResponse = UpdateResourceDefinitionResponse'
  { _urdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdrsResponseStatus' - -- | The response status code.
updateResourceDefinitionResponse
    :: Int -- ^ 'urdrsResponseStatus'
    -> UpdateResourceDefinitionResponse
updateResourceDefinitionResponse pResponseStatus_ =
  UpdateResourceDefinitionResponse' {_urdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
urdrsResponseStatus :: Lens' UpdateResourceDefinitionResponse Int
urdrsResponseStatus = lens _urdrsResponseStatus (\ s a -> s{_urdrsResponseStatus = a})

instance NFData UpdateResourceDefinitionResponse
         where
