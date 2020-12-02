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
-- Module      : Network.AWS.Greengrass.DeleteCoreDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a core definition.
module Network.AWS.Greengrass.DeleteCoreDefinition
    (
    -- * Creating a Request
      deleteCoreDefinition
    , DeleteCoreDefinition
    -- * Request Lenses
    , dcdCoreDefinitionId

    -- * Destructuring the Response
    , deleteCoreDefinitionResponse
    , DeleteCoreDefinitionResponse
    -- * Response Lenses
    , dcdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteCoreDefinition' smart constructor.
newtype DeleteCoreDefinition = DeleteCoreDefinition'
  { _dcdCoreDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCoreDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdCoreDefinitionId' - The ID of the core definition.
deleteCoreDefinition
    :: Text -- ^ 'dcdCoreDefinitionId'
    -> DeleteCoreDefinition
deleteCoreDefinition pCoreDefinitionId_ =
  DeleteCoreDefinition' {_dcdCoreDefinitionId = pCoreDefinitionId_}


-- | The ID of the core definition.
dcdCoreDefinitionId :: Lens' DeleteCoreDefinition Text
dcdCoreDefinitionId = lens _dcdCoreDefinitionId (\ s a -> s{_dcdCoreDefinitionId = a})

instance AWSRequest DeleteCoreDefinition where
        type Rs DeleteCoreDefinition =
             DeleteCoreDefinitionResponse
        request = delete greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteCoreDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteCoreDefinition where

instance NFData DeleteCoreDefinition where

instance ToHeaders DeleteCoreDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteCoreDefinition where
        toPath DeleteCoreDefinition'{..}
          = mconcat
              ["/greengrass/definition/cores/",
               toBS _dcdCoreDefinitionId]

instance ToQuery DeleteCoreDefinition where
        toQuery = const mempty

-- | /See:/ 'deleteCoreDefinitionResponse' smart constructor.
newtype DeleteCoreDefinitionResponse = DeleteCoreDefinitionResponse'
  { _dcdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCoreDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcdrsResponseStatus' - -- | The response status code.
deleteCoreDefinitionResponse
    :: Int -- ^ 'dcdrsResponseStatus'
    -> DeleteCoreDefinitionResponse
deleteCoreDefinitionResponse pResponseStatus_ =
  DeleteCoreDefinitionResponse' {_dcdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcdrsResponseStatus :: Lens' DeleteCoreDefinitionResponse Int
dcdrsResponseStatus = lens _dcdrsResponseStatus (\ s a -> s{_dcdrsResponseStatus = a})

instance NFData DeleteCoreDefinitionResponse where
