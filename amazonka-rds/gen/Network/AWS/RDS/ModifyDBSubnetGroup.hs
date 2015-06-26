{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
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

-- | Modifies an existing DB subnet group. DB subnet groups must contain at
-- least one subnet in at least two AZs in the region.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ModifyDBSubnetGroup.html>
module Network.AWS.RDS.ModifyDBSubnetGroup
    (
    -- * Request
      ModifyDBSubnetGroup
    -- ** Request constructor
    , modifyDBSubnetGroup
    -- ** Request lenses
    , mdsgDBSubnetGroupDescription
    , mdsgDBSubnetGroupName
    , mdsgSubnetIds

    -- * Response
    , ModifyDBSubnetGroupResponse
    -- ** Response constructor
    , modifyDBSubnetGroupResponse
    -- ** Response lenses
    , mdsgrDBSubnetGroup
    , mdsgrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- |
--
-- /See:/ 'modifyDBSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdsgDBSubnetGroupDescription'
--
-- * 'mdsgDBSubnetGroupName'
--
-- * 'mdsgSubnetIds'
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'{_mdsgDBSubnetGroupDescription :: Maybe Text, _mdsgDBSubnetGroupName :: Text, _mdsgSubnetIds :: [Text]} deriving (Eq, Read, Show)

-- | 'ModifyDBSubnetGroup' smart constructor.
modifyDBSubnetGroup :: Text -> ModifyDBSubnetGroup
modifyDBSubnetGroup pDBSubnetGroupName = ModifyDBSubnetGroup'{_mdsgDBSubnetGroupDescription = Nothing, _mdsgDBSubnetGroupName = pDBSubnetGroupName, _mdsgSubnetIds = mempty};

-- | The description for the DB subnet group.
mdsgDBSubnetGroupDescription :: Lens' ModifyDBSubnetGroup (Maybe Text)
mdsgDBSubnetGroupDescription = lens _mdsgDBSubnetGroupDescription (\ s a -> s{_mdsgDBSubnetGroupDescription = a});

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens. Must not be \"Default\".
--
-- Example: @mySubnetgroup@
mdsgDBSubnetGroupName :: Lens' ModifyDBSubnetGroup Text
mdsgDBSubnetGroupName = lens _mdsgDBSubnetGroupName (\ s a -> s{_mdsgDBSubnetGroupName = a});

-- | The EC2 subnet IDs for the DB subnet group.
mdsgSubnetIds :: Lens' ModifyDBSubnetGroup [Text]
mdsgSubnetIds = lens _mdsgSubnetIds (\ s a -> s{_mdsgSubnetIds = a});

instance AWSRequest ModifyDBSubnetGroup where
        type Sv ModifyDBSubnetGroup = RDS
        type Rs ModifyDBSubnetGroup =
             ModifyDBSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "ModifyDBSubnetGroupResult"
              (\ s h x ->
                 ModifyDBSubnetGroupResponse' <$>
                   (x .@? "DBSubnetGroup") <*> (pure (fromEnum s)))

instance ToHeaders ModifyDBSubnetGroup where
        toHeaders = const mempty

instance ToPath ModifyDBSubnetGroup where
        toPath = const "/"

instance ToQuery ModifyDBSubnetGroup where
        toQuery ModifyDBSubnetGroup'{..}
          = mconcat
              ["Action" =: ("ModifyDBSubnetGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSubnetGroupDescription" =:
                 _mdsgDBSubnetGroupDescription,
               "DBSubnetGroupName" =: _mdsgDBSubnetGroupName,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _mdsgSubnetIds]

-- | /See:/ 'modifyDBSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdsgrDBSubnetGroup'
--
-- * 'mdsgrStatusCode'
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'{_mdsgrDBSubnetGroup :: Maybe DBSubnetGroup, _mdsgrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ModifyDBSubnetGroupResponse' smart constructor.
modifyDBSubnetGroupResponse :: Int -> ModifyDBSubnetGroupResponse
modifyDBSubnetGroupResponse pStatusCode = ModifyDBSubnetGroupResponse'{_mdsgrDBSubnetGroup = Nothing, _mdsgrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
mdsgrDBSubnetGroup :: Lens' ModifyDBSubnetGroupResponse (Maybe DBSubnetGroup)
mdsgrDBSubnetGroup = lens _mdsgrDBSubnetGroup (\ s a -> s{_mdsgrDBSubnetGroup = a});

-- | FIXME: Undocumented member.
mdsgrStatusCode :: Lens' ModifyDBSubnetGroupResponse Int
mdsgrStatusCode = lens _mdsgrStatusCode (\ s a -> s{_mdsgrStatusCode = a});
