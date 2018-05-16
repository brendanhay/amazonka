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
-- Module      : Network.AWS.AutoScaling.CreateOrUpdateTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates tags for the specified Auto Scaling group.
--
--
-- When you specify a tag with a key that already exists, the operation overwrites the previous tag definition, and you do not get an error message.
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/autoscaling-tagging.html Tagging Auto Scaling Groups and Instances> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.CreateOrUpdateTags
    (
    -- * Creating a Request
      createOrUpdateTags
    , CreateOrUpdateTags
    -- * Request Lenses
    , coutTags

    -- * Destructuring the Response
    , createOrUpdateTagsResponse
    , CreateOrUpdateTagsResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createOrUpdateTags' smart constructor.
newtype CreateOrUpdateTags = CreateOrUpdateTags'
  { _coutTags :: [Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOrUpdateTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coutTags' - One or more tags.
createOrUpdateTags
    :: CreateOrUpdateTags
createOrUpdateTags = CreateOrUpdateTags' {_coutTags = mempty}


-- | One or more tags.
coutTags :: Lens' CreateOrUpdateTags [Tag]
coutTags = lens _coutTags (\ s a -> s{_coutTags = a}) . _Coerce

instance AWSRequest CreateOrUpdateTags where
        type Rs CreateOrUpdateTags =
             CreateOrUpdateTagsResponse
        request = postQuery autoScaling
        response = receiveNull CreateOrUpdateTagsResponse'

instance Hashable CreateOrUpdateTags where

instance NFData CreateOrUpdateTags where

instance ToHeaders CreateOrUpdateTags where
        toHeaders = const mempty

instance ToPath CreateOrUpdateTags where
        toPath = const "/"

instance ToQuery CreateOrUpdateTags where
        toQuery CreateOrUpdateTags'{..}
          = mconcat
              ["Action" =: ("CreateOrUpdateTags" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Tags" =: toQueryList "member" _coutTags]

-- | /See:/ 'createOrUpdateTagsResponse' smart constructor.
data CreateOrUpdateTagsResponse =
  CreateOrUpdateTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOrUpdateTagsResponse' with the minimum fields required to make a request.
--
createOrUpdateTagsResponse
    :: CreateOrUpdateTagsResponse
createOrUpdateTagsResponse = CreateOrUpdateTagsResponse'


instance NFData CreateOrUpdateTagsResponse where
