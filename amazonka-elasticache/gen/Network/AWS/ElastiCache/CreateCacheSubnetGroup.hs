{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /CreateCacheSubnetGroup/ action creates a new cache subnet group.
--
-- Use this parameter only when you are creating a cluster in an Amazon
-- Virtual Private Cloud (VPC).
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html>
module Network.AWS.ElastiCache.CreateCacheSubnetGroup
    (
    -- * Request
      CreateCacheSubnetGroup
    -- ** Request constructor
    , createCacheSubnetGroup
    -- ** Request lenses
    , ccsgrqCacheSubnetGroupName
    , ccsgrqCacheSubnetGroupDescription
    , ccsgrqSubnetIds

    -- * Response
    , CreateCacheSubnetGroupResponse
    -- ** Response constructor
    , createCacheSubnetGroupResponse
    -- ** Response lenses
    , crsCacheSubnetGroup
    , crsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CreateCacheSubnetGroup/ action.
--
-- /See:/ 'createCacheSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrqCacheSubnetGroupName'
--
-- * 'ccsgrqCacheSubnetGroupDescription'
--
-- * 'ccsgrqSubnetIds'
data CreateCacheSubnetGroup = CreateCacheSubnetGroup'
    { _ccsgrqCacheSubnetGroupName        :: !Text
    , _ccsgrqCacheSubnetGroupDescription :: !Text
    , _ccsgrqSubnetIds                   :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheSubnetGroup' smart constructor.
createCacheSubnetGroup :: Text -> Text -> CreateCacheSubnetGroup
createCacheSubnetGroup pCacheSubnetGroupName_ pCacheSubnetGroupDescription_ =
    CreateCacheSubnetGroup'
    { _ccsgrqCacheSubnetGroupName = pCacheSubnetGroupName_
    , _ccsgrqCacheSubnetGroupDescription = pCacheSubnetGroupDescription_
    , _ccsgrqSubnetIds = mempty
    }

-- | A name for the cache subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
--
-- Example: @mysubnetgroup@
ccsgrqCacheSubnetGroupName :: Lens' CreateCacheSubnetGroup Text
ccsgrqCacheSubnetGroupName = lens _ccsgrqCacheSubnetGroupName (\ s a -> s{_ccsgrqCacheSubnetGroupName = a});

-- | A description for the cache subnet group.
ccsgrqCacheSubnetGroupDescription :: Lens' CreateCacheSubnetGroup Text
ccsgrqCacheSubnetGroupDescription = lens _ccsgrqCacheSubnetGroupDescription (\ s a -> s{_ccsgrqCacheSubnetGroupDescription = a});

-- | A list of VPC subnet IDs for the cache subnet group.
ccsgrqSubnetIds :: Lens' CreateCacheSubnetGroup [Text]
ccsgrqSubnetIds = lens _ccsgrqSubnetIds (\ s a -> s{_ccsgrqSubnetIds = a});

instance AWSRequest CreateCacheSubnetGroup where
        type Sv CreateCacheSubnetGroup = ElastiCache
        type Rs CreateCacheSubnetGroup =
             CreateCacheSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateCacheSubnetGroupResult"
              (\ s h x ->
                 CreateCacheSubnetGroupResponse' <$>
                   (x .@? "CacheSubnetGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateCacheSubnetGroup where
        toHeaders = const mempty

instance ToPath CreateCacheSubnetGroup where
        toPath = const "/"

instance ToQuery CreateCacheSubnetGroup where
        toQuery CreateCacheSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateCacheSubnetGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSubnetGroupName" =:
                 _ccsgrqCacheSubnetGroupName,
               "CacheSubnetGroupDescription" =:
                 _ccsgrqCacheSubnetGroupDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _ccsgrqSubnetIds]

-- | /See:/ 'createCacheSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsCacheSubnetGroup'
--
-- * 'crsStatus'
data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse'
    { _crsCacheSubnetGroup :: !(Maybe CacheSubnetGroup)
    , _crsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheSubnetGroupResponse' smart constructor.
createCacheSubnetGroupResponse :: Int -> CreateCacheSubnetGroupResponse
createCacheSubnetGroupResponse pStatus_ =
    CreateCacheSubnetGroupResponse'
    { _crsCacheSubnetGroup = Nothing
    , _crsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
crsCacheSubnetGroup :: Lens' CreateCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
crsCacheSubnetGroup = lens _crsCacheSubnetGroup (\ s a -> s{_crsCacheSubnetGroup = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CreateCacheSubnetGroupResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
