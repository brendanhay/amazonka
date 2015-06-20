{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
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

-- | Creates a new DB subnet group. DB subnet groups must contain at least
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
    , cdbsgTags
    , cdbsgDBSubnetGroupName
    , cdbsgDBSubnetGroupDescription
    , cdbsgSubnetIds

    -- * Response
    , CreateDBSubnetGroupResponse
    -- ** Response constructor
    , createDBSubnetGroupResponse
    -- ** Response lenses
    , cdsgrDBSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDBSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsgTags'
--
-- * 'cdbsgDBSubnetGroupName'
--
-- * 'cdbsgDBSubnetGroupDescription'
--
-- * 'cdbsgSubnetIds'
data CreateDBSubnetGroup = CreateDBSubnetGroup'{_cdbsgTags :: Maybe [Tag], _cdbsgDBSubnetGroupName :: Text, _cdbsgDBSubnetGroupDescription :: Text, _cdbsgSubnetIds :: [Text]} deriving (Eq, Read, Show)

-- | 'CreateDBSubnetGroup' smart constructor.
createDBSubnetGroup :: Text -> Text -> CreateDBSubnetGroup
createDBSubnetGroup pDBSubnetGroupName pDBSubnetGroupDescription = CreateDBSubnetGroup'{_cdbsgTags = Nothing, _cdbsgDBSubnetGroupName = pDBSubnetGroupName, _cdbsgDBSubnetGroupDescription = pDBSubnetGroupDescription, _cdbsgSubnetIds = mempty};

-- | FIXME: Undocumented member.
cdbsgTags :: Lens' CreateDBSubnetGroup [Tag]
cdbsgTags = lens _cdbsgTags (\ s a -> s{_cdbsgTags = a}) . _Default;

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens. Must not be \"Default\".
--
-- Example: @mySubnetgroup@
cdbsgDBSubnetGroupName :: Lens' CreateDBSubnetGroup Text
cdbsgDBSubnetGroupName = lens _cdbsgDBSubnetGroupName (\ s a -> s{_cdbsgDBSubnetGroupName = a});

-- | The description for the DB subnet group.
cdbsgDBSubnetGroupDescription :: Lens' CreateDBSubnetGroup Text
cdbsgDBSubnetGroupDescription = lens _cdbsgDBSubnetGroupDescription (\ s a -> s{_cdbsgDBSubnetGroupDescription = a});

-- | The EC2 Subnet IDs for the DB subnet group.
cdbsgSubnetIds :: Lens' CreateDBSubnetGroup [Text]
cdbsgSubnetIds = lens _cdbsgSubnetIds (\ s a -> s{_cdbsgSubnetIds = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest CreateDBSubnetGroup where
        type Sv CreateDBSubnetGroup = RDS
        type Rs CreateDBSubnetGroup =
             CreateDBSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBSubnetGroupResult"
              (\ s h x ->
                 CreateDBSubnetGroupResponse' <$>
                   (x .@? "DBSubnetGroup"))

instance ToHeaders CreateDBSubnetGroup where
        toHeaders = const mempty

instance ToPath CreateDBSubnetGroup where
        toPath = const "/"

instance ToQuery CreateDBSubnetGroup where
        toQuery CreateDBSubnetGroup'{..}
          = mconcat
              ["Action" =: ("CreateDBSubnetGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbsgTags),
               "DBSubnetGroupName" =: _cdbsgDBSubnetGroupName,
               "DBSubnetGroupDescription" =:
                 _cdbsgDBSubnetGroupDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _cdbsgSubnetIds]

-- | /See:/ 'createDBSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsgrDBSubnetGroup'
newtype CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'{_cdsgrDBSubnetGroup :: Maybe DBSubnetGroup} deriving (Eq, Read, Show)

-- | 'CreateDBSubnetGroupResponse' smart constructor.
createDBSubnetGroupResponse :: CreateDBSubnetGroupResponse
createDBSubnetGroupResponse = CreateDBSubnetGroupResponse'{_cdsgrDBSubnetGroup = Nothing};

-- | FIXME: Undocumented member.
cdsgrDBSubnetGroup :: Lens' CreateDBSubnetGroupResponse (Maybe DBSubnetGroup)
cdsgrDBSubnetGroup = lens _cdsgrDBSubnetGroup (\ s a -> s{_cdsgrDBSubnetGroup = a});
