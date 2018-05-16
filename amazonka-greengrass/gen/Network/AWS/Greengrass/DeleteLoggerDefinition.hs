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
-- Module      : Network.AWS.Greengrass.DeleteLoggerDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logger definition.
module Network.AWS.Greengrass.DeleteLoggerDefinition
    (
    -- * Creating a Request
      deleteLoggerDefinition
    , DeleteLoggerDefinition
    -- * Request Lenses
    , dldLoggerDefinitionId

    -- * Destructuring the Response
    , deleteLoggerDefinitionResponse
    , DeleteLoggerDefinitionResponse
    -- * Response Lenses
    , dldrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLoggerDefinition' smart constructor.
newtype DeleteLoggerDefinition = DeleteLoggerDefinition'
  { _dldLoggerDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoggerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldLoggerDefinitionId' - The ID of the logger definition.
deleteLoggerDefinition
    :: Text -- ^ 'dldLoggerDefinitionId'
    -> DeleteLoggerDefinition
deleteLoggerDefinition pLoggerDefinitionId_ =
  DeleteLoggerDefinition' {_dldLoggerDefinitionId = pLoggerDefinitionId_}


-- | The ID of the logger definition.
dldLoggerDefinitionId :: Lens' DeleteLoggerDefinition Text
dldLoggerDefinitionId = lens _dldLoggerDefinitionId (\ s a -> s{_dldLoggerDefinitionId = a})

instance AWSRequest DeleteLoggerDefinition where
        type Rs DeleteLoggerDefinition =
             DeleteLoggerDefinitionResponse
        request = delete greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteLoggerDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteLoggerDefinition where

instance NFData DeleteLoggerDefinition where

instance ToHeaders DeleteLoggerDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteLoggerDefinition where
        toPath DeleteLoggerDefinition'{..}
          = mconcat
              ["/greengrass/definition/loggers/",
               toBS _dldLoggerDefinitionId]

instance ToQuery DeleteLoggerDefinition where
        toQuery = const mempty

-- | /See:/ 'deleteLoggerDefinitionResponse' smart constructor.
newtype DeleteLoggerDefinitionResponse = DeleteLoggerDefinitionResponse'
  { _dldrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoggerDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dldrsResponseStatus' - -- | The response status code.
deleteLoggerDefinitionResponse
    :: Int -- ^ 'dldrsResponseStatus'
    -> DeleteLoggerDefinitionResponse
deleteLoggerDefinitionResponse pResponseStatus_ =
  DeleteLoggerDefinitionResponse' {_dldrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dldrsResponseStatus :: Lens' DeleteLoggerDefinitionResponse Int
dldrsResponseStatus = lens _dldrsResponseStatus (\ s a -> s{_dldrsResponseStatus = a})

instance NFData DeleteLoggerDefinitionResponse where
