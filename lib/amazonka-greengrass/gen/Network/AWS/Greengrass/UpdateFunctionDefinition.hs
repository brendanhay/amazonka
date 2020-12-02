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
-- Module      : Network.AWS.Greengrass.UpdateFunctionDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a Lambda function definition.
module Network.AWS.Greengrass.UpdateFunctionDefinition
    (
    -- * Creating a Request
      updateFunctionDefinition
    , UpdateFunctionDefinition
    -- * Request Lenses
    , ufdName
    , ufdFunctionDefinitionId

    -- * Destructuring the Response
    , updateFunctionDefinitionResponse
    , UpdateFunctionDefinitionResponse
    -- * Response Lenses
    , ufdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFunctionDefinition' smart constructor.
data UpdateFunctionDefinition = UpdateFunctionDefinition'
  { _ufdName                 :: !(Maybe Text)
  , _ufdFunctionDefinitionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFunctionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufdName' - The name of the definition.
--
-- * 'ufdFunctionDefinitionId' - The ID of the Lambda function definition.
updateFunctionDefinition
    :: Text -- ^ 'ufdFunctionDefinitionId'
    -> UpdateFunctionDefinition
updateFunctionDefinition pFunctionDefinitionId_ =
  UpdateFunctionDefinition'
    {_ufdName = Nothing, _ufdFunctionDefinitionId = pFunctionDefinitionId_}


-- | The name of the definition.
ufdName :: Lens' UpdateFunctionDefinition (Maybe Text)
ufdName = lens _ufdName (\ s a -> s{_ufdName = a})

-- | The ID of the Lambda function definition.
ufdFunctionDefinitionId :: Lens' UpdateFunctionDefinition Text
ufdFunctionDefinitionId = lens _ufdFunctionDefinitionId (\ s a -> s{_ufdFunctionDefinitionId = a})

instance AWSRequest UpdateFunctionDefinition where
        type Rs UpdateFunctionDefinition =
             UpdateFunctionDefinitionResponse
        request = putJSON greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateFunctionDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateFunctionDefinition where

instance NFData UpdateFunctionDefinition where

instance ToHeaders UpdateFunctionDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateFunctionDefinition where
        toJSON UpdateFunctionDefinition'{..}
          = object (catMaybes [("Name" .=) <$> _ufdName])

instance ToPath UpdateFunctionDefinition where
        toPath UpdateFunctionDefinition'{..}
          = mconcat
              ["/greengrass/definition/functions/",
               toBS _ufdFunctionDefinitionId]

instance ToQuery UpdateFunctionDefinition where
        toQuery = const mempty

-- | /See:/ 'updateFunctionDefinitionResponse' smart constructor.
newtype UpdateFunctionDefinitionResponse = UpdateFunctionDefinitionResponse'
  { _ufdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufdrsResponseStatus' - -- | The response status code.
updateFunctionDefinitionResponse
    :: Int -- ^ 'ufdrsResponseStatus'
    -> UpdateFunctionDefinitionResponse
updateFunctionDefinitionResponse pResponseStatus_ =
  UpdateFunctionDefinitionResponse' {_ufdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ufdrsResponseStatus :: Lens' UpdateFunctionDefinitionResponse Int
ufdrsResponseStatus = lens _ufdrsResponseStatus (\ s a -> s{_ufdrsResponseStatus = a})

instance NFData UpdateFunctionDefinitionResponse
         where
