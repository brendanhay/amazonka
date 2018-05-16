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
-- Module      : Network.AWS.AutoScaling.DeleteTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified tags.
--
--
module Network.AWS.AutoScaling.DeleteTags
    (
    -- * Creating a Request
      deleteTags
    , DeleteTags
    -- * Request Lenses
    , dtTags

    -- * Destructuring the Response
    , deleteTagsResponse
    , DeleteTagsResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTags' smart constructor.
newtype DeleteTags = DeleteTags'
  { _dtTags :: [Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTags' - One or more tags.
deleteTags
    :: DeleteTags
deleteTags = DeleteTags' {_dtTags = mempty}


-- | One or more tags.
dtTags :: Lens' DeleteTags [Tag]
dtTags = lens _dtTags (\ s a -> s{_dtTags = a}) . _Coerce

instance AWSRequest DeleteTags where
        type Rs DeleteTags = DeleteTagsResponse
        request = postQuery autoScaling
        response = receiveNull DeleteTagsResponse'

instance Hashable DeleteTags where

instance NFData DeleteTags where

instance ToHeaders DeleteTags where
        toHeaders = const mempty

instance ToPath DeleteTags where
        toPath = const "/"

instance ToQuery DeleteTags where
        toQuery DeleteTags'{..}
          = mconcat
              ["Action" =: ("DeleteTags" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "Tags" =: toQueryList "member" _dtTags]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
  DeleteTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteTagsResponse' with the minimum fields required to make a request.
--
deleteTagsResponse
    :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'


instance NFData DeleteTagsResponse where
