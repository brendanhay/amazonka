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
-- Module      : Network.AWS.CloudDirectory.UpdateSchema
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schema name with a new name. Only development schema names can be updated.
--
--
module Network.AWS.CloudDirectory.UpdateSchema
    (
    -- * Creating a Request
      updateSchema
    , UpdateSchema
    -- * Request Lenses
    , usSchemaARN
    , usName

    -- * Destructuring the Response
    , updateSchemaResponse
    , UpdateSchemaResponse
    -- * Response Lenses
    , usrsSchemaARN
    , usrsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSchema' smart constructor.
data UpdateSchema = UpdateSchema'
  { _usSchemaARN :: !Text
  , _usName      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSchema' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usSchemaARN' - The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
--
-- * 'usName' - The name of the schema.
updateSchema
    :: Text -- ^ 'usSchemaARN'
    -> Text -- ^ 'usName'
    -> UpdateSchema
updateSchema pSchemaARN_ pName_ =
  UpdateSchema' {_usSchemaARN = pSchemaARN_, _usName = pName_}


-- | The Amazon Resource Name (ARN) of the development schema. For more information, see 'arns' .
usSchemaARN :: Lens' UpdateSchema Text
usSchemaARN = lens _usSchemaARN (\ s a -> s{_usSchemaARN = a})

-- | The name of the schema.
usName :: Lens' UpdateSchema Text
usName = lens _usName (\ s a -> s{_usName = a})

instance AWSRequest UpdateSchema where
        type Rs UpdateSchema = UpdateSchemaResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSchemaResponse' <$>
                   (x .?> "SchemaArn") <*> (pure (fromEnum s)))

instance Hashable UpdateSchema where

instance NFData UpdateSchema where

instance ToHeaders UpdateSchema where
        toHeaders UpdateSchema'{..}
          = mconcat ["x-amz-data-partition" =# _usSchemaARN]

instance ToJSON UpdateSchema where
        toJSON UpdateSchema'{..}
          = object (catMaybes [Just ("Name" .= _usName)])

instance ToPath UpdateSchema where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/schema/update"

instance ToQuery UpdateSchema where
        toQuery = const mempty

-- | /See:/ 'updateSchemaResponse' smart constructor.
data UpdateSchemaResponse = UpdateSchemaResponse'
  { _usrsSchemaARN      :: !(Maybe Text)
  , _usrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSchemaResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usrsSchemaARN' - The ARN that is associated with the updated schema. For more information, see 'arns' .
--
-- * 'usrsResponseStatus' - -- | The response status code.
updateSchemaResponse
    :: Int -- ^ 'usrsResponseStatus'
    -> UpdateSchemaResponse
updateSchemaResponse pResponseStatus_ =
  UpdateSchemaResponse'
    {_usrsSchemaARN = Nothing, _usrsResponseStatus = pResponseStatus_}


-- | The ARN that is associated with the updated schema. For more information, see 'arns' .
usrsSchemaARN :: Lens' UpdateSchemaResponse (Maybe Text)
usrsSchemaARN = lens _usrsSchemaARN (\ s a -> s{_usrsSchemaARN = a})

-- | -- | The response status code.
usrsResponseStatus :: Lens' UpdateSchemaResponse Int
usrsResponseStatus = lens _usrsResponseStatus (\ s a -> s{_usrsResponseStatus = a})

instance NFData UpdateSchemaResponse where
