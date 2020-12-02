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
-- Module      : Network.AWS.Greengrass.UpdateLoggerDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a logger definition.
module Network.AWS.Greengrass.UpdateLoggerDefinition
    (
    -- * Creating a Request
      updateLoggerDefinition
    , UpdateLoggerDefinition
    -- * Request Lenses
    , uldName
    , uldLoggerDefinitionId

    -- * Destructuring the Response
    , updateLoggerDefinitionResponse
    , UpdateLoggerDefinitionResponse
    -- * Response Lenses
    , uldrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateLoggerDefinition' smart constructor.
data UpdateLoggerDefinition = UpdateLoggerDefinition'
  { _uldName               :: !(Maybe Text)
  , _uldLoggerDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLoggerDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uldName' - The name of the definition.
--
-- * 'uldLoggerDefinitionId' - The ID of the logger definition.
updateLoggerDefinition
    :: Text -- ^ 'uldLoggerDefinitionId'
    -> UpdateLoggerDefinition
updateLoggerDefinition pLoggerDefinitionId_ =
  UpdateLoggerDefinition'
    {_uldName = Nothing, _uldLoggerDefinitionId = pLoggerDefinitionId_}


-- | The name of the definition.
uldName :: Lens' UpdateLoggerDefinition (Maybe Text)
uldName = lens _uldName (\ s a -> s{_uldName = a})

-- | The ID of the logger definition.
uldLoggerDefinitionId :: Lens' UpdateLoggerDefinition Text
uldLoggerDefinitionId = lens _uldLoggerDefinitionId (\ s a -> s{_uldLoggerDefinitionId = a})

instance AWSRequest UpdateLoggerDefinition where
        type Rs UpdateLoggerDefinition =
             UpdateLoggerDefinitionResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateLoggerDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateLoggerDefinition where

instance NFData UpdateLoggerDefinition where

instance ToHeaders UpdateLoggerDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateLoggerDefinition where
        toJSON UpdateLoggerDefinition'{..}
          = object (catMaybes [("Name" .=) <$> _uldName])

instance ToPath UpdateLoggerDefinition where
        toPath UpdateLoggerDefinition'{..}
          = mconcat
              ["/greengrass/definition/loggers/",
               toBS _uldLoggerDefinitionId]

instance ToQuery UpdateLoggerDefinition where
        toQuery = const mempty

-- | /See:/ 'updateLoggerDefinitionResponse' smart constructor.
newtype UpdateLoggerDefinitionResponse = UpdateLoggerDefinitionResponse'
  { _uldrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateLoggerDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uldrsResponseStatus' - -- | The response status code.
updateLoggerDefinitionResponse
    :: Int -- ^ 'uldrsResponseStatus'
    -> UpdateLoggerDefinitionResponse
updateLoggerDefinitionResponse pResponseStatus_ =
  UpdateLoggerDefinitionResponse' {_uldrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uldrsResponseStatus :: Lens' UpdateLoggerDefinitionResponse Int
uldrsResponseStatus = lens _uldrsResponseStatus (\ s a -> s{_uldrsResponseStatus = a})

instance NFData UpdateLoggerDefinitionResponse where
