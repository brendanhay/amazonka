{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /CreateCacheSecurityGroup/ action creates a new cache security
-- group. Use a cache security group to control access to one or more cache
-- clusters.
--
-- Cache security groups are only used when you are creating a cache
-- cluster outside of an Amazon Virtual Private Cloud (VPC). If you are
-- creating a cache cluster inside of a VPC, use a cache subnet group
-- instead. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSubnetGroup.html CreateCacheSubnetGroup>.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheSecurityGroup.html>
module Network.AWS.ElastiCache.CreateCacheSecurityGroup
    (
    -- * Request
      CreateCacheSecurityGroup
    -- ** Request constructor
    , createCacheSecurityGroup
    -- ** Request lenses
    , ccsgrqCacheSecurityGroupName
    , ccsgrqDescription

    -- * Response
    , CreateCacheSecurityGroupResponse
    -- ** Response constructor
    , createCacheSecurityGroupResponse
    -- ** Response lenses
    , ccsgrsCacheSecurityGroup
    , ccsgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CreateCacheSecurityGroup/ action.
--
-- /See:/ 'createCacheSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrqCacheSecurityGroupName'
--
-- * 'ccsgrqDescription'
data CreateCacheSecurityGroup = CreateCacheSecurityGroup'
    { _ccsgrqCacheSecurityGroupName :: !Text
    , _ccsgrqDescription            :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheSecurityGroup' smart constructor.
createCacheSecurityGroup :: Text -> Text -> CreateCacheSecurityGroup
createCacheSecurityGroup pCacheSecurityGroupName_ pDescription_ =
    CreateCacheSecurityGroup'
    { _ccsgrqCacheSecurityGroupName = pCacheSecurityGroupName_
    , _ccsgrqDescription = pDescription_
    }

-- | A name for the cache security group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters.
-- Cannot be the word \"Default\".
--
-- Example: @mysecuritygroup@
ccsgrqCacheSecurityGroupName :: Lens' CreateCacheSecurityGroup Text
ccsgrqCacheSecurityGroupName = lens _ccsgrqCacheSecurityGroupName (\ s a -> s{_ccsgrqCacheSecurityGroupName = a});

-- | A description for the cache security group.
ccsgrqDescription :: Lens' CreateCacheSecurityGroup Text
ccsgrqDescription = lens _ccsgrqDescription (\ s a -> s{_ccsgrqDescription = a});

instance AWSRequest CreateCacheSecurityGroup where
        type Sv CreateCacheSecurityGroup = ElastiCache
        type Rs CreateCacheSecurityGroup =
             CreateCacheSecurityGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateCacheSecurityGroupResult"
              (\ s h x ->
                 CreateCacheSecurityGroupResponse' <$>
                   (x .@? "CacheSecurityGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateCacheSecurityGroup where
        toHeaders = const mempty

instance ToPath CreateCacheSecurityGroup where
        toPath = const "/"

instance ToQuery CreateCacheSecurityGroup where
        toQuery CreateCacheSecurityGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateCacheSecurityGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _ccsgrqCacheSecurityGroupName,
               "Description" =: _ccsgrqDescription]

-- | /See:/ 'createCacheSecurityGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrsCacheSecurityGroup'
--
-- * 'ccsgrsStatus'
data CreateCacheSecurityGroupResponse = CreateCacheSecurityGroupResponse'
    { _ccsgrsCacheSecurityGroup :: !(Maybe CacheSecurityGroup)
    , _ccsgrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheSecurityGroupResponse' smart constructor.
createCacheSecurityGroupResponse :: Int -> CreateCacheSecurityGroupResponse
createCacheSecurityGroupResponse pStatus_ =
    CreateCacheSecurityGroupResponse'
    { _ccsgrsCacheSecurityGroup = Nothing
    , _ccsgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
ccsgrsCacheSecurityGroup :: Lens' CreateCacheSecurityGroupResponse (Maybe CacheSecurityGroup)
ccsgrsCacheSecurityGroup = lens _ccsgrsCacheSecurityGroup (\ s a -> s{_ccsgrsCacheSecurityGroup = a});

-- | FIXME: Undocumented member.
ccsgrsStatus :: Lens' CreateCacheSecurityGroupResponse Int
ccsgrsStatus = lens _ccsgrsStatus (\ s a -> s{_ccsgrsStatus = a});
