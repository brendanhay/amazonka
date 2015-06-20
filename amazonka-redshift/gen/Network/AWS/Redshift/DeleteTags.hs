{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DeleteTags
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a tag or tags from a resource. You must provide the ARN of the
-- resource from which you want to delete the tag or tags.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteTags.html>
module Network.AWS.Redshift.DeleteTags
    (
    -- * Request
      DeleteTags
    -- ** Request constructor
    , deleteTags
    -- ** Request lenses
    , delResourceName
    , delTagKeys

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delResourceName'
--
-- * 'delTagKeys'
data DeleteTags = DeleteTags'{_delResourceName :: Text, _delTagKeys :: [Text]} deriving (Eq, Read, Show)

-- | 'DeleteTags' smart constructor.
deleteTags :: Text -> DeleteTags
deleteTags pResourceName = DeleteTags'{_delResourceName = pResourceName, _delTagKeys = mempty};

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@.
delResourceName :: Lens' DeleteTags Text
delResourceName = lens _delResourceName (\ s a -> s{_delResourceName = a});

-- | The tag key that you want to delete.
delTagKeys :: Lens' DeleteTags [Text]
delTagKeys = lens _delTagKeys (\ s a -> s{_delTagKeys = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteTags where
        type Sv DeleteTags = Redshift
        type Rs DeleteTags = DeleteTagsResponse
        request = post
        response = receiveNull DeleteTagsResponse'

instance ToHeaders DeleteTags where
        toHeaders = const mempty

instance ToPath DeleteTags where
        toPath = const "/"

instance ToQuery DeleteTags where
        toQuery DeleteTags'{..}
          = mconcat
              ["Action" =: ("DeleteTags" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ResourceName" =: _delResourceName,
               "TagKeys" =: toQueryList "TagKey" _delTagKeys]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse = DeleteTagsResponse' deriving (Eq, Read, Show)

-- | 'DeleteTagsResponse' smart constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse';
