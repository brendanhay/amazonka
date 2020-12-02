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
-- Module      : Network.AWS.CloudDirectory.DeleteSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a given schema. Schemas in a development and published state can only be deleted.
--
--
module Network.AWS.CloudDirectory.DeleteSchema
    (
    -- * Creating a Request
      deleteSchema
    , DeleteSchema
    -- * Request Lenses
    , dsSchemaARN

    -- * Destructuring the Response
    , deleteSchemaResponse
    , DeleteSchemaResponse
    -- * Response Lenses
    , dsrsSchemaARN
    , dsrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSchema' smart constructor.
newtype DeleteSchema = DeleteSchema'
  { _dsSchemaARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsSchemaARN' - The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
deleteSchema
    :: Text -- ^ 'dsSchemaARN'
    -> DeleteSchema
deleteSchema pSchemaARN_ = DeleteSchema' {_dsSchemaARN = pSchemaARN_}


-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
dsSchemaARN :: Lens' DeleteSchema Text
dsSchemaARN = lens _dsSchemaARN (\ s a -> s{_dsSchemaARN = a})

instance AWSRequest DeleteSchema where
        type Rs DeleteSchema = DeleteSchemaResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSchemaResponse' <$>
                   (x .?> "SchemaArn") <*> (pure (fromEnum s)))

instance Hashable DeleteSchema where

instance NFData DeleteSchema where

instance ToHeaders DeleteSchema where
        toHeaders DeleteSchema'{..}
          = mconcat ["x-amz-data-partition" =# _dsSchemaARN]

instance ToJSON DeleteSchema where
        toJSON = const (Object mempty)

instance ToPath DeleteSchema where
        toPath
          = const "/amazonclouddirectory/2017-01-11/schema"

instance ToQuery DeleteSchema where
        toQuery = const mempty

-- | /See:/ 'deleteSchemaResponse' smart constructor.
data DeleteSchemaResponse = DeleteSchemaResponse'
  { _dsrsSchemaARN      :: !(Maybe Text)
  , _dsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsSchemaARN' - The input ARN that is returned as part of the response. For more information, see 'arns' .
--
-- * 'dsrsResponseStatus' - -- | The response status code.
deleteSchemaResponse
    :: Int -- ^ 'dsrsResponseStatus'
    -> DeleteSchemaResponse
deleteSchemaResponse pResponseStatus_ =
  DeleteSchemaResponse'
    {_dsrsSchemaARN = Nothing, _dsrsResponseStatus = pResponseStatus_}


-- | The input ARN that is returned as part of the response. For more information, see 'arns' .
dsrsSchemaARN :: Lens' DeleteSchemaResponse (Maybe Text)
dsrsSchemaARN = lens _dsrsSchemaARN (\ s a -> s{_dsrsSchemaARN = a})

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DeleteSchemaResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\ s a -> s{_dsrsResponseStatus = a})

instance NFData DeleteSchemaResponse where
