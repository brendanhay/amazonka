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
-- Module      : Network.AWS.Glue.UpdateUserDefinedFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing function definition in the Data Catalog.
--
--
module Network.AWS.Glue.UpdateUserDefinedFunction
    (
    -- * Creating a Request
      updateUserDefinedFunction
    , UpdateUserDefinedFunction
    -- * Request Lenses
    , uudfCatalogId
    , uudfDatabaseName
    , uudfFunctionName
    , uudfFunctionInput

    -- * Destructuring the Response
    , updateUserDefinedFunctionResponse
    , UpdateUserDefinedFunctionResponse
    -- * Response Lenses
    , uudfrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateUserDefinedFunction' smart constructor.
data UpdateUserDefinedFunction = UpdateUserDefinedFunction'
  { _uudfCatalogId     :: !(Maybe Text)
  , _uudfDatabaseName  :: !Text
  , _uudfFunctionName  :: !Text
  , _uudfFunctionInput :: !UserDefinedFunctionInput
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserDefinedFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uudfCatalogId' - The ID of the Data Catalog where the function to be updated is located. If none is supplied, the AWS account ID is used by default.
--
-- * 'uudfDatabaseName' - The name of the catalog database where the function to be updated is located.
--
-- * 'uudfFunctionName' - The name of the function.
--
-- * 'uudfFunctionInput' - A @FunctionInput@ object that re-defines the function in the Data Catalog.
updateUserDefinedFunction
    :: Text -- ^ 'uudfDatabaseName'
    -> Text -- ^ 'uudfFunctionName'
    -> UserDefinedFunctionInput -- ^ 'uudfFunctionInput'
    -> UpdateUserDefinedFunction
updateUserDefinedFunction pDatabaseName_ pFunctionName_ pFunctionInput_ =
  UpdateUserDefinedFunction'
    { _uudfCatalogId = Nothing
    , _uudfDatabaseName = pDatabaseName_
    , _uudfFunctionName = pFunctionName_
    , _uudfFunctionInput = pFunctionInput_
    }


-- | The ID of the Data Catalog where the function to be updated is located. If none is supplied, the AWS account ID is used by default.
uudfCatalogId :: Lens' UpdateUserDefinedFunction (Maybe Text)
uudfCatalogId = lens _uudfCatalogId (\ s a -> s{_uudfCatalogId = a})

-- | The name of the catalog database where the function to be updated is located.
uudfDatabaseName :: Lens' UpdateUserDefinedFunction Text
uudfDatabaseName = lens _uudfDatabaseName (\ s a -> s{_uudfDatabaseName = a})

-- | The name of the function.
uudfFunctionName :: Lens' UpdateUserDefinedFunction Text
uudfFunctionName = lens _uudfFunctionName (\ s a -> s{_uudfFunctionName = a})

-- | A @FunctionInput@ object that re-defines the function in the Data Catalog.
uudfFunctionInput :: Lens' UpdateUserDefinedFunction UserDefinedFunctionInput
uudfFunctionInput = lens _uudfFunctionInput (\ s a -> s{_uudfFunctionInput = a})

instance AWSRequest UpdateUserDefinedFunction where
        type Rs UpdateUserDefinedFunction =
             UpdateUserDefinedFunctionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateUserDefinedFunctionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateUserDefinedFunction where

instance NFData UpdateUserDefinedFunction where

instance ToHeaders UpdateUserDefinedFunction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.UpdateUserDefinedFunction" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserDefinedFunction where
        toJSON UpdateUserDefinedFunction'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _uudfCatalogId,
                  Just ("DatabaseName" .= _uudfDatabaseName),
                  Just ("FunctionName" .= _uudfFunctionName),
                  Just ("FunctionInput" .= _uudfFunctionInput)])

instance ToPath UpdateUserDefinedFunction where
        toPath = const "/"

instance ToQuery UpdateUserDefinedFunction where
        toQuery = const mempty

-- | /See:/ 'updateUserDefinedFunctionResponse' smart constructor.
newtype UpdateUserDefinedFunctionResponse = UpdateUserDefinedFunctionResponse'
  { _uudfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uudfrsResponseStatus' - -- | The response status code.
updateUserDefinedFunctionResponse
    :: Int -- ^ 'uudfrsResponseStatus'
    -> UpdateUserDefinedFunctionResponse
updateUserDefinedFunctionResponse pResponseStatus_ =
  UpdateUserDefinedFunctionResponse' {_uudfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uudfrsResponseStatus :: Lens' UpdateUserDefinedFunctionResponse Int
uudfrsResponseStatus = lens _uudfrsResponseStatus (\ s a -> s{_uudfrsResponseStatus = a})

instance NFData UpdateUserDefinedFunctionResponse
         where
