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
-- Module      : Network.AWS.Greengrass.DeleteFunctionDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lambda function definition.
module Network.AWS.Greengrass.DeleteFunctionDefinition
    (
    -- * Creating a Request
      deleteFunctionDefinition
    , DeleteFunctionDefinition
    -- * Request Lenses
    , dfdFunctionDefinitionId

    -- * Destructuring the Response
    , deleteFunctionDefinitionResponse
    , DeleteFunctionDefinitionResponse
    -- * Response Lenses
    , dfdrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFunctionDefinition' smart constructor.
newtype DeleteFunctionDefinition = DeleteFunctionDefinition'
  { _dfdFunctionDefinitionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFunctionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfdFunctionDefinitionId' - The ID of the Lambda function definition.
deleteFunctionDefinition
    :: Text -- ^ 'dfdFunctionDefinitionId'
    -> DeleteFunctionDefinition
deleteFunctionDefinition pFunctionDefinitionId_ =
  DeleteFunctionDefinition' {_dfdFunctionDefinitionId = pFunctionDefinitionId_}


-- | The ID of the Lambda function definition.
dfdFunctionDefinitionId :: Lens' DeleteFunctionDefinition Text
dfdFunctionDefinitionId = lens _dfdFunctionDefinitionId (\ s a -> s{_dfdFunctionDefinitionId = a})

instance AWSRequest DeleteFunctionDefinition where
        type Rs DeleteFunctionDefinition =
             DeleteFunctionDefinitionResponse
        request = delete greengrass
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteFunctionDefinitionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteFunctionDefinition where

instance NFData DeleteFunctionDefinition where

instance ToHeaders DeleteFunctionDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteFunctionDefinition where
        toPath DeleteFunctionDefinition'{..}
          = mconcat
              ["/greengrass/definition/functions/",
               toBS _dfdFunctionDefinitionId]

instance ToQuery DeleteFunctionDefinition where
        toQuery = const mempty

-- | /See:/ 'deleteFunctionDefinitionResponse' smart constructor.
newtype DeleteFunctionDefinitionResponse = DeleteFunctionDefinitionResponse'
  { _dfdrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFunctionDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfdrsResponseStatus' - -- | The response status code.
deleteFunctionDefinitionResponse
    :: Int -- ^ 'dfdrsResponseStatus'
    -> DeleteFunctionDefinitionResponse
deleteFunctionDefinitionResponse pResponseStatus_ =
  DeleteFunctionDefinitionResponse' {_dfdrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dfdrsResponseStatus :: Lens' DeleteFunctionDefinitionResponse Int
dfdrsResponseStatus = lens _dfdrsResponseStatus (\ s a -> s{_dfdrsResponseStatus = a})

instance NFData DeleteFunctionDefinitionResponse
         where
