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
-- Module      : Network.AWS.Greengrass.DeleteResourceDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource definition.
module Network.AWS.Greengrass.DeleteResourceDefinition
    (
    -- * Creating a Request
      deleteResourceDefinition
    , DeleteResourceDefinition
    -- * Request Lenses
    , drdResourceDefinitionId

    -- * Destructuring the Response
    , deleteResourceDefinitionResponse
    , DeleteResourceDefinitionResponse
    -- * Response Lenses
    , drdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteResourceDefinition' smart constructor.
newtype DeleteResourceDefinition = DeleteResourceDefinition'
  { _drdResourceDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourceDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdResourceDefinitionId' - The ID of the resource definition.
deleteResourceDefinition
    :: Text -- ^ 'drdResourceDefinitionId'
    -> DeleteResourceDefinition
deleteResourceDefinition pResourceDefinitionId_ =
  DeleteResourceDefinition' {_drdResourceDefinitionId = pResourceDefinitionId_}


-- | The ID of the resource definition.
drdResourceDefinitionId :: Lens' DeleteResourceDefinition Text
drdResourceDefinitionId = lens _drdResourceDefinitionId (\ s a -> s{_drdResourceDefinitionId = a})

instance AWSRequest DeleteResourceDefinition where
        type Rs DeleteResourceDefinition =
             DeleteResourceDefinitionResponse
        request = delete greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteResourceDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteResourceDefinition where

instance NFData DeleteResourceDefinition where

instance ToHeaders DeleteResourceDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteResourceDefinition where
        toPath DeleteResourceDefinition'{..}
          = mconcat
              ["/greengrass/definition/resources/",
               toBS _drdResourceDefinitionId]

instance ToQuery DeleteResourceDefinition where
        toQuery = const mempty

-- | /See:/ 'deleteResourceDefinitionResponse' smart constructor.
newtype DeleteResourceDefinitionResponse = DeleteResourceDefinitionResponse'
  { _drdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteResourceDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdrsResponseStatus' - -- | The response status code.
deleteResourceDefinitionResponse
    :: Int -- ^ 'drdrsResponseStatus'
    -> DeleteResourceDefinitionResponse
deleteResourceDefinitionResponse pResponseStatus_ =
  DeleteResourceDefinitionResponse' {_drdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drdrsResponseStatus :: Lens' DeleteResourceDefinitionResponse Int
drdrsResponseStatus = lens _drdrsResponseStatus (\ s a -> s{_drdrsResponseStatus = a})

instance NFData DeleteResourceDefinitionResponse
         where
