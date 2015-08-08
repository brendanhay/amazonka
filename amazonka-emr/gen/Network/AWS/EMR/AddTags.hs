{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddTags
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to an Amazon EMR resource. Tags make it easier to associate
-- clusters in various ways, such as grouping clusters to track your Amazon
-- EMR resource allocation costs. For more information, see
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/DeveloperGuide/emr-plan-tags.html Tagging Amazon EMR Resources>.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddTags.html AWS API Reference> for AddTags.
module Network.AWS.EMR.AddTags
    (
    -- * Creating a Request
      AddTags
    , addTags
    -- * Request Lenses
    , atResourceId
    , atTags

    -- * Destructuring the Response
    , AddTagsResponse
    , addTagsResponse
    -- * Response Lenses
    , atrsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input identifies a cluster and a list of tags to attach.
--
-- /See:/ 'addTags' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atResourceId'
--
-- * 'atTags'
data AddTags = AddTags'
    { _atResourceId :: !Text
    , _atTags       :: ![Tag]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTags' smart constructor.
addTags :: Text -> AddTags
addTags pResourceId_ =
    AddTags'
    { _atResourceId = pResourceId_
    , _atTags = mempty
    }

-- | The Amazon EMR resource identifier to which tags will be added. This
-- value must be a cluster identifier.
atResourceId :: Lens' AddTags Text
atResourceId = lens _atResourceId (\ s a -> s{_atResourceId = a});

-- | A list of tags to associate with a cluster and propagate to Amazon EC2
-- instances. Tags are user-defined key\/value pairs that consist of a
-- required key string with a maximum of 128 characters, and an optional
-- value string with a maximum of 256 characters.
atTags :: Lens' AddTags [Tag]
atTags = lens _atTags (\ s a -> s{_atTags = a}) . _Coerce;

instance AWSRequest AddTags where
        type Sv AddTags = EMR
        type Rs AddTags = AddTagsResponse
        request = postJSON
        response
          = receiveEmpty
              (\ s h x -> AddTagsResponse' <$> (pure (fromEnum s)))

instance ToHeaders AddTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.AddTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddTags where
        toJSON AddTags'{..}
          = object
              ["ResourceId" .= _atResourceId, "Tags" .= _atTags]

instance ToPath AddTags where
        toPath = const "/"

instance ToQuery AddTags where
        toQuery = const mempty

-- | This output indicates the result of adding tags to a resource.
--
-- /See:/ 'addTagsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'atrsStatus'
newtype AddTagsResponse = AddTagsResponse'
    { _atrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddTagsResponse' smart constructor.
addTagsResponse :: Int -> AddTagsResponse
addTagsResponse pStatus_ =
    AddTagsResponse'
    { _atrsStatus = pStatus_
    }

-- | Undocumented member.
atrsStatus :: Lens' AddTagsResponse Int
atrsStatus = lens _atrsStatus (\ s a -> s{_atrsStatus = a});
