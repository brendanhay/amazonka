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
-- Module      : Network.AWS.CloudDirectory.CreateDirectory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 'Directory' by copying the published schema into the directory. A directory cannot be created without a schema.
--
--
module Network.AWS.CloudDirectory.CreateDirectory
    (
    -- * Creating a Request
      createDirectory
    , CreateDirectory
    -- * Request Lenses
    , cdName
    , cdSchemaARN

    -- * Destructuring the Response
    , createDirectoryResponse
    , CreateDirectoryResponse
    -- * Response Lenses
    , cdrsResponseStatus
    , cdrsDirectoryARN
    , cdrsName
    , cdrsObjectIdentifier
    , cdrsAppliedSchemaARN
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDirectory' smart constructor.
data CreateDirectory = CreateDirectory'
  { _cdName      :: !Text
  , _cdSchemaARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdName' - The name of the 'Directory' . Should be unique per account, per region.
--
-- * 'cdSchemaARN' - The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
createDirectory
    :: Text -- ^ 'cdName'
    -> Text -- ^ 'cdSchemaARN'
    -> CreateDirectory
createDirectory pName_ pSchemaARN_ =
  CreateDirectory' {_cdName = pName_, _cdSchemaARN = pSchemaARN_}


-- | The name of the 'Directory' . Should be unique per account, per region.
cdName :: Lens' CreateDirectory Text
cdName = lens _cdName (\ s a -> s{_cdName = a})

-- | The Amazon Resource Name (ARN) of the published schema that will be copied into the data 'Directory' . For more information, see 'arns' .
cdSchemaARN :: Lens' CreateDirectory Text
cdSchemaARN = lens _cdSchemaARN (\ s a -> s{_cdSchemaARN = a})

instance AWSRequest CreateDirectory where
        type Rs CreateDirectory = CreateDirectoryResponse
        request = putJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 CreateDirectoryResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "DirectoryArn") <*>
                     (x .:> "Name")
                     <*> (x .:> "ObjectIdentifier")
                     <*> (x .:> "AppliedSchemaArn"))

instance Hashable CreateDirectory where

instance NFData CreateDirectory where

instance ToHeaders CreateDirectory where
        toHeaders CreateDirectory'{..}
          = mconcat ["x-amz-data-partition" =# _cdSchemaARN]

instance ToJSON CreateDirectory where
        toJSON CreateDirectory'{..}
          = object (catMaybes [Just ("Name" .= _cdName)])

instance ToPath CreateDirectory where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/directory/create"

instance ToQuery CreateDirectory where
        toQuery = const mempty

-- | /See:/ 'createDirectoryResponse' smart constructor.
data CreateDirectoryResponse = CreateDirectoryResponse'
  { _cdrsResponseStatus   :: !Int
  , _cdrsDirectoryARN     :: !Text
  , _cdrsName             :: !Text
  , _cdrsObjectIdentifier :: !Text
  , _cdrsAppliedSchemaARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDirectoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsResponseStatus' - -- | The response status code.
--
-- * 'cdrsDirectoryARN' - The ARN that is associated with the 'Directory' . For more information, see 'arns' .
--
-- * 'cdrsName' - The name of the 'Directory' .
--
-- * 'cdrsObjectIdentifier' - The root object node of the created directory.
--
-- * 'cdrsAppliedSchemaARN' - The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
createDirectoryResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> Text -- ^ 'cdrsDirectoryARN'
    -> Text -- ^ 'cdrsName'
    -> Text -- ^ 'cdrsObjectIdentifier'
    -> Text -- ^ 'cdrsAppliedSchemaARN'
    -> CreateDirectoryResponse
createDirectoryResponse pResponseStatus_ pDirectoryARN_ pName_ pObjectIdentifier_ pAppliedSchemaARN_ =
  CreateDirectoryResponse'
    { _cdrsResponseStatus = pResponseStatus_
    , _cdrsDirectoryARN = pDirectoryARN_
    , _cdrsName = pName_
    , _cdrsObjectIdentifier = pObjectIdentifier_
    , _cdrsAppliedSchemaARN = pAppliedSchemaARN_
    }


-- | -- | The response status code.
cdrsResponseStatus :: Lens' CreateDirectoryResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

-- | The ARN that is associated with the 'Directory' . For more information, see 'arns' .
cdrsDirectoryARN :: Lens' CreateDirectoryResponse Text
cdrsDirectoryARN = lens _cdrsDirectoryARN (\ s a -> s{_cdrsDirectoryARN = a})

-- | The name of the 'Directory' .
cdrsName :: Lens' CreateDirectoryResponse Text
cdrsName = lens _cdrsName (\ s a -> s{_cdrsName = a})

-- | The root object node of the created directory.
cdrsObjectIdentifier :: Lens' CreateDirectoryResponse Text
cdrsObjectIdentifier = lens _cdrsObjectIdentifier (\ s a -> s{_cdrsObjectIdentifier = a})

-- | The ARN of the published schema in the 'Directory' . Once a published schema is copied into the directory, it has its own ARN, which is referred to applied schema ARN. For more information, see 'arns' .
cdrsAppliedSchemaARN :: Lens' CreateDirectoryResponse Text
cdrsAppliedSchemaARN = lens _cdrsAppliedSchemaARN (\ s a -> s{_cdrsAppliedSchemaARN = a})

instance NFData CreateDirectoryResponse where
