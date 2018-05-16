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
-- Module      : Network.AWS.Glue.DeleteUserDefinedFunction
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing function definition from the Data Catalog.
--
--
module Network.AWS.Glue.DeleteUserDefinedFunction
    (
    -- * Creating a Request
      deleteUserDefinedFunction
    , DeleteUserDefinedFunction
    -- * Request Lenses
    , dudfCatalogId
    , dudfDatabaseName
    , dudfFunctionName

    -- * Destructuring the Response
    , deleteUserDefinedFunctionResponse
    , DeleteUserDefinedFunctionResponse
    -- * Response Lenses
    , dudfrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserDefinedFunction' smart constructor.
data DeleteUserDefinedFunction = DeleteUserDefinedFunction'
  { _dudfCatalogId    :: !(Maybe Text)
  , _dudfDatabaseName :: !Text
  , _dudfFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserDefinedFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dudfCatalogId' - The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
--
-- * 'dudfDatabaseName' - The name of the catalog database where the function is located.
--
-- * 'dudfFunctionName' - The name of the function definition to be deleted.
deleteUserDefinedFunction
    :: Text -- ^ 'dudfDatabaseName'
    -> Text -- ^ 'dudfFunctionName'
    -> DeleteUserDefinedFunction
deleteUserDefinedFunction pDatabaseName_ pFunctionName_ =
  DeleteUserDefinedFunction'
    { _dudfCatalogId = Nothing
    , _dudfDatabaseName = pDatabaseName_
    , _dudfFunctionName = pFunctionName_
    }


-- | The ID of the Data Catalog where the function to be deleted is located. If none is supplied, the AWS account ID is used by default.
dudfCatalogId :: Lens' DeleteUserDefinedFunction (Maybe Text)
dudfCatalogId = lens _dudfCatalogId (\ s a -> s{_dudfCatalogId = a})

-- | The name of the catalog database where the function is located.
dudfDatabaseName :: Lens' DeleteUserDefinedFunction Text
dudfDatabaseName = lens _dudfDatabaseName (\ s a -> s{_dudfDatabaseName = a})

-- | The name of the function definition to be deleted.
dudfFunctionName :: Lens' DeleteUserDefinedFunction Text
dudfFunctionName = lens _dudfFunctionName (\ s a -> s{_dudfFunctionName = a})

instance AWSRequest DeleteUserDefinedFunction where
        type Rs DeleteUserDefinedFunction =
             DeleteUserDefinedFunctionResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteUserDefinedFunctionResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteUserDefinedFunction where

instance NFData DeleteUserDefinedFunction where

instance ToHeaders DeleteUserDefinedFunction where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.DeleteUserDefinedFunction" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteUserDefinedFunction where
        toJSON DeleteUserDefinedFunction'{..}
          = object
              (catMaybes
                 [("CatalogId" .=) <$> _dudfCatalogId,
                  Just ("DatabaseName" .= _dudfDatabaseName),
                  Just ("FunctionName" .= _dudfFunctionName)])

instance ToPath DeleteUserDefinedFunction where
        toPath = const "/"

instance ToQuery DeleteUserDefinedFunction where
        toQuery = const mempty

-- | /See:/ 'deleteUserDefinedFunctionResponse' smart constructor.
newtype DeleteUserDefinedFunctionResponse = DeleteUserDefinedFunctionResponse'
  { _dudfrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserDefinedFunctionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dudfrsResponseStatus' - -- | The response status code.
deleteUserDefinedFunctionResponse
    :: Int -- ^ 'dudfrsResponseStatus'
    -> DeleteUserDefinedFunctionResponse
deleteUserDefinedFunctionResponse pResponseStatus_ =
  DeleteUserDefinedFunctionResponse' {_dudfrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dudfrsResponseStatus :: Lens' DeleteUserDefinedFunctionResponse Int
dudfrsResponseStatus = lens _dudfrsResponseStatus (\ s a -> s{_dudfrsResponseStatus = a})

instance NFData DeleteUserDefinedFunctionResponse
         where
