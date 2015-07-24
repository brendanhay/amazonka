{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a tag or tags from a resource. You must provide the ARN of the
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
    , dResourceName
    , dTagKeys

    -- * Response
    , DeleteTagsResponse
    -- ** Response constructor
    , deleteTagsResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the output from the @DeleteTags@ action.
--
-- /See:/ 'deleteTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dResourceName'
--
-- * 'dTagKeys'
data DeleteTags = DeleteTags'
    { _dResourceName :: !Text
    , _dTagKeys      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTags' smart constructor.
deleteTags :: Text -> DeleteTags
deleteTags pResourceName_ =
    DeleteTags'
    { _dResourceName = pResourceName_
    , _dTagKeys = mempty
    }

-- | The Amazon Resource Name (ARN) from which you want to remove the tag or
-- tags. For example, @arn:aws:redshift:us-east-1:123456789:cluster:t1@.
dResourceName :: Lens' DeleteTags Text
dResourceName = lens _dResourceName (\ s a -> s{_dResourceName = a});

-- | The tag key that you want to delete.
dTagKeys :: Lens' DeleteTags [Text]
dTagKeys = lens _dTagKeys (\ s a -> s{_dTagKeys = a});

instance AWSRequest DeleteTags where
        type Sv DeleteTags = Redshift
        type Rs DeleteTags = DeleteTagsResponse
        request = post "DeleteTags"
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
               "ResourceName" =: _dResourceName,
               "TagKeys" =: toQueryList "TagKey" _dTagKeys]

-- | /See:/ 'deleteTagsResponse' smart constructor.
data DeleteTagsResponse =
    DeleteTagsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTagsResponse' smart constructor.
deleteTagsResponse :: DeleteTagsResponse
deleteTagsResponse = DeleteTagsResponse'
