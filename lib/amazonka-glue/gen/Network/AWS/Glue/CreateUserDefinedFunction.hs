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
-- Module      : Network.AWS.Glue.CreateUserDefinedFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new function definition in the Data Catalog.
--
--
module Network.AWS.Glue.CreateUserDefinedFunction
    (
    -- * Creating a Request
      createUserDefinedFunction
    , CreateUserDefinedFunction
    -- * Request Lenses
    , cudfCatalogId
    , cudfDatabaseName
    , cudfFunctionInput

    -- * Destructuring the Response
    , createUserDefinedFunctionResponse
    , CreateUserDefinedFunctionResponse
    -- * Response Lenses
    , cudfrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUserDefinedFunction' smart constructor.
data CreateUserDefinedFunction = CreateUserDefinedFunction'
  { _cudfCatalogId     :: !(Maybe Text)
  , _cudfDatabaseName  :: !Text
  , _cudfFunctionInput :: !UserDefinedFunctionInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserDefinedFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cudfCatalogId' - The ID of the Data Catalog in which to create the function. If none is supplied, the AWS account ID is used by default.
--
-- * 'cudfDatabaseName' - The name of the catalog database in which to create the function.
--
-- * 'cudfFunctionInput' - A @FunctionInput@ object that defines the function to create in the Data Catalog.
createUserDefinedFunction
    :: Text -- ^ 'cudfDatabaseName'
    -> UserDefinedFunctionInput -- ^ 'cudfFunctionInput'
    -> CreateUserDefinedFunction
createUserDefinedFunction pDatabaseName_ pFunctionInput_ =
  CreateUserDefinedFunction'
    { _cudfCatalogId = Nothing
    , _cudfDatabaseName = pDatabaseName_
    , _cudfFunctionInput = pFunctionInput_
    }


-- | The ID of the Data Catalog in which to create the function. If none is supplied, the AWS account ID is used by default.
cudfCatalogId :: Lens' CreateUserDefinedFunction (Maybe Text)
cudfCatalogId = lens _cudfCatalogId (\ s a -> s{_cudfCatalogId = a})

-- | The name of the catalog database in which to create the function.
cudfDatabaseName :: Lens' CreateUserDefinedFunction Text
cudfDatabaseName = lens _cudfDatabaseName (\ s a -> s{_cudfDatabaseName = a})

-- | A @FunctionInput@ object that defines the function to create in the Data Catalog.
cudfFunctionInput :: Lens' CreateUserDefinedFunction UserDefinedFunctionInput
cudfFunctionInput = lens _cudfFunctionInput (\ s a -> s{_cudfFunctionInput = a})

instance AWSRequest CreateUserDefinedFunction where
        type Rs CreateUserDefinedFunction =
             CreateUserDefinedFunctionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 CreateUserDefinedFunctionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateUserDefinedFunction where

instance NFData CreateUserDefinedFunction where

instance ToHeaders CreateUserDefinedFunction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateUserDefinedFunction" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserDefinedFunction where
        toJSON CreateUserDefinedFunction'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _cudfCatalogId,
                  Just ("DatabaseName" .= _cudfDatabaseName),
                  Just ("FunctionInput" .= _cudfFunctionInput)])

instance ToPath CreateUserDefinedFunction where
        toPath = const "/"

instance ToQuery CreateUserDefinedFunction where
        toQuery = const mempty

-- | /See:/ 'createUserDefinedFunctionResponse' smart constructor.
newtype CreateUserDefinedFunctionResponse = CreateUserDefinedFunctionResponse'
  { _cudfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cudfrsResponseStatus' - -- | The response status code.
createUserDefinedFunctionResponse
    :: Int -- ^ 'cudfrsResponseStatus'
    -> CreateUserDefinedFunctionResponse
createUserDefinedFunctionResponse pResponseStatus_ =
  CreateUserDefinedFunctionResponse' {_cudfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cudfrsResponseStatus :: Lens' CreateUserDefinedFunctionResponse Int
cudfrsResponseStatus = lens _cudfrsResponseStatus (\ s a -> s{_cudfrsResponseStatus = a})

instance NFData CreateUserDefinedFunctionResponse
         where
