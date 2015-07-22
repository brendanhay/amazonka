{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB subnet group. DB subnet groups must contain at least
-- one subnet in at least two AZs in the region.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSubnetGroup.html>
module Network.AWS.RDS.CreateDBSubnetGroup
    (
    -- * Request
      CreateDBSubnetGroup
    -- ** Request constructor
    , createDBSubnetGroup
    -- ** Request lenses
    , cdbsgrqTags
    , cdbsgrqDBSubnetGroupName
    , cdbsgrqDBSubnetGroupDescription
    , cdbsgrqSubnetIds

    -- * Response
    , CreateDBSubnetGroupResponse
    -- ** Response constructor
    , createDBSubnetGroupResponse
    -- ** Response lenses
    , cdbsgrsDBSubnetGroup
    , cdbsgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgrqTags'
--
-- * 'cdbsgrqDBSubnetGroupName'
--
-- * 'cdbsgrqDBSubnetGroupDescription'
--
-- * 'cdbsgrqSubnetIds'
data CreateDBSubnetGroup = CreateDBSubnetGroup'
    { _cdbsgrqTags                     :: !(Maybe [Tag])
    , _cdbsgrqDBSubnetGroupName        :: !Text
    , _cdbsgrqDBSubnetGroupDescription :: !Text
    , _cdbsgrqSubnetIds                :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSubnetGroup' smart constructor.
createDBSubnetGroup :: Text -> Text -> CreateDBSubnetGroup
createDBSubnetGroup pDBSubnetGroupName pDBSubnetGroupDescription =
    CreateDBSubnetGroup'
    { _cdbsgrqTags = Nothing
    , _cdbsgrqDBSubnetGroupName = pDBSubnetGroupName
    , _cdbsgrqDBSubnetGroupDescription = pDBSubnetGroupDescription
    , _cdbsgrqSubnetIds = mempty
    }

-- | FIXME: Undocumented member.
cdbsgrqTags :: Lens' CreateDBSubnetGroup [Tag]
cdbsgrqTags = lens _cdbsgrqTags (\ s a -> s{_cdbsgrqTags = a}) . _Default;

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens. Must not be \"Default\".
--
-- Example: @mySubnetgroup@
cdbsgrqDBSubnetGroupName :: Lens' CreateDBSubnetGroup Text
cdbsgrqDBSubnetGroupName = lens _cdbsgrqDBSubnetGroupName (\ s a -> s{_cdbsgrqDBSubnetGroupName = a});

-- | The description for the DB subnet group.
cdbsgrqDBSubnetGroupDescription :: Lens' CreateDBSubnetGroup Text
cdbsgrqDBSubnetGroupDescription = lens _cdbsgrqDBSubnetGroupDescription (\ s a -> s{_cdbsgrqDBSubnetGroupDescription = a});

-- | The EC2 Subnet IDs for the DB subnet group.
cdbsgrqSubnetIds :: Lens' CreateDBSubnetGroup [Text]
cdbsgrqSubnetIds = lens _cdbsgrqSubnetIds (\ s a -> s{_cdbsgrqSubnetIds = a});

instance AWSRequest CreateDBSubnetGroup where
        type Sv CreateDBSubnetGroup = RDS
        type Rs CreateDBSubnetGroup =
             CreateDBSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBSubnetGroupResult"
              (\ s h x ->
                 CreateDBSubnetGroupResponse' <$>
                   (x .@? "DBSubnetGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateDBSubnetGroup where
        toHeaders = const mempty

instance ToPath CreateDBSubnetGroup where
        toPath = const "/"

instance ToQuery CreateDBSubnetGroup where
        toQuery CreateDBSubnetGroup'{..}
          = mconcat
              ["Action" =: ("CreateDBSubnetGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cdbsgrqTags),
               "DBSubnetGroupName" =: _cdbsgrqDBSubnetGroupName,
               "DBSubnetGroupDescription" =:
                 _cdbsgrqDBSubnetGroupDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _cdbsgrqSubnetIds]

-- | /See:/ 'createDBSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgrsDBSubnetGroup'
--
-- * 'cdbsgrsStatus'
data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'
    { _cdbsgrsDBSubnetGroup :: !(Maybe DBSubnetGroup)
    , _cdbsgrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSubnetGroupResponse' smart constructor.
createDBSubnetGroupResponse :: Int -> CreateDBSubnetGroupResponse
createDBSubnetGroupResponse pStatus =
    CreateDBSubnetGroupResponse'
    { _cdbsgrsDBSubnetGroup = Nothing
    , _cdbsgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdbsgrsDBSubnetGroup :: Lens' CreateDBSubnetGroupResponse (Maybe DBSubnetGroup)
cdbsgrsDBSubnetGroup = lens _cdbsgrsDBSubnetGroup (\ s a -> s{_cdbsgrsDBSubnetGroup = a});

-- | FIXME: Undocumented member.
cdbsgrsStatus :: Lens' CreateDBSubnetGroupResponse Int
cdbsgrsStatus = lens _cdbsgrsStatus (\ s a -> s{_cdbsgrsStatus = a});
