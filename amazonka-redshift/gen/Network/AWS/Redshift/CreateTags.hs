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
-- Module      : Network.AWS.Redshift.CreateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more tags to a specified resource.
--
--
-- A resource can have up to 10 tags. If you try to create more than 10 tags for a resource, you will receive an error and the attempt will fail.
--
-- If you specify a key that already exists for the resource, the value for that key will be updated with the new value.
--
module Network.AWS.Redshift.CreateTags
    (
    -- * Creating a Request
      createTags
    , CreateTags
    -- * Request Lenses
    , ctResourceName
    , ctTags

    -- * Destructuring the Response
    , createTagsResponse
    , CreateTagsResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the output from the @CreateTags@ action.
--
--
--
-- /See:/ 'createTags' smart constructor.
data CreateTags = CreateTags'
  { _ctResourceName :: !Text
  , _ctTags         :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctResourceName' - The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@ .
--
-- * 'ctTags' - One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ .
createTags
    :: Text -- ^ 'ctResourceName'
    -> CreateTags
createTags pResourceName_ =
  CreateTags' {_ctResourceName = pResourceName_, _ctTags = mempty}


-- | The Amazon Resource Name (ARN) to which you want to add the tag or tags. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@ .
ctResourceName :: Lens' CreateTags Text
ctResourceName = lens _ctResourceName (\ s a -> s{_ctResourceName = a})

-- | One or more name/value pairs to add as tags to the specified resource. Each tag name is passed in with the parameter @Key@ and the corresponding value is passed in with the parameter @Value@ . The @Key@ and @Value@ parameters are separated by a comma (,). Separate multiple tags with a space. For example, @--tags "Key"="owner","Value"="admin" "Key"="environment","Value"="test" "Key"="version","Value"="1.0"@ .
ctTags :: Lens' CreateTags [Tag]
ctTags = lens _ctTags (\ s a -> s{_ctTags = a}) . _Coerce

instance AWSRequest CreateTags where
        type Rs CreateTags = CreateTagsResponse
        request = postQuery redshift
        response = receiveNull CreateTagsResponse'

instance Hashable CreateTags where

instance NFData CreateTags where

instance ToHeaders CreateTags where
        toHeaders = const mempty

instance ToPath CreateTags where
        toPath = const "/"

instance ToQuery CreateTags where
        toQuery CreateTags'{..}
          = mconcat
              ["Action" =: ("CreateTags" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ResourceName" =: _ctResourceName,
               "Tags" =: toQueryList "Tag" _ctTags]

-- | /See:/ 'createTagsResponse' smart constructor.
data CreateTagsResponse =
  CreateTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateTagsResponse' with the minimum fields required to make a request.
--
createTagsResponse
    :: CreateTagsResponse
createTagsResponse = CreateTagsResponse'


instance NFData CreateTagsResponse where
