{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveTagsFromResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes metadata tags from an Amazon RDS resource.
--
-- For an overview on tagging an Amazon RDS resource, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources>.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RemoveTagsFromResource.html>
module Network.AWS.RDS.RemoveTagsFromResource
    (
    -- * Request
      RemoveTagsFromResource
    -- ** Request constructor
    , removeTagsFromResource
    -- ** Request lenses
    , rtfrResourceName
    , rtfrTagKeys

    -- * Response
    , RemoveTagsFromResourceResponse
    -- ** Response constructor
    , removeTagsFromResourceResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'removeTagsFromResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rtfrResourceName'
--
-- * 'rtfrTagKeys'
data RemoveTagsFromResource = RemoveTagsFromResource'
    { _rtfrResourceName :: !Text
    , _rtfrTagKeys      :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromResource' smart constructor.
removeTagsFromResource :: Text -> RemoveTagsFromResource
removeTagsFromResource pResourceName =
    RemoveTagsFromResource'
    { _rtfrResourceName = pResourceName
    , _rtfrTagKeys = mempty
    }

-- | The Amazon RDS resource the tags will be removed from. This value is an
-- Amazon Resource Name (ARN). For information about creating an ARN, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing an RDS Amazon Resource Name (ARN)>.
rtfrResourceName :: Lens' RemoveTagsFromResource Text
rtfrResourceName = lens _rtfrResourceName (\ s a -> s{_rtfrResourceName = a});

-- | The tag key (name) of the tag to be removed.
rtfrTagKeys :: Lens' RemoveTagsFromResource [Text]
rtfrTagKeys = lens _rtfrTagKeys (\ s a -> s{_rtfrTagKeys = a});

instance AWSRequest RemoveTagsFromResource where
        type Sv RemoveTagsFromResource = RDS
        type Rs RemoveTagsFromResource =
             RemoveTagsFromResourceResponse
        request = post
        response
          = receiveNull RemoveTagsFromResourceResponse'

instance ToHeaders RemoveTagsFromResource where
        toHeaders = const mempty

instance ToPath RemoveTagsFromResource where
        toPath = const "/"

instance ToQuery RemoveTagsFromResource where
        toQuery RemoveTagsFromResource'{..}
          = mconcat
              ["Action" =:
                 ("RemoveTagsFromResource" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "ResourceName" =: _rtfrResourceName,
               "TagKeys" =: toQueryList "member" _rtfrTagKeys]

-- | /See:/ 'removeTagsFromResourceResponse' smart constructor.
data RemoveTagsFromResourceResponse =
    RemoveTagsFromResourceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemoveTagsFromResourceResponse' smart constructor.
removeTagsFromResourceResponse :: RemoveTagsFromResourceResponse
removeTagsFromResourceResponse = RemoveTagsFromResourceResponse'
