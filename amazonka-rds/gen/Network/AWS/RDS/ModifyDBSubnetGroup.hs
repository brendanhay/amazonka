{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing DB subnet group. DB subnet groups must contain at
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
    , mdsgrqDBSubnetGroupDescription
    , mdsgrqDBSubnetGroupName
    , mdsgrqSubnetIds

    -- * Response
    , ModifyDBSubnetGroupResponse
    -- ** Response constructor
    , modifyDBSubnetGroupResponse
    -- ** Response lenses
    , mdsgrsDBSubnetGroup
    , mdsgrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'modifyDBSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdsgrqDBSubnetGroupDescription'
--
-- * 'mdsgrqDBSubnetGroupName'
--
-- * 'mdsgrqSubnetIds'
data ModifyDBSubnetGroup = ModifyDBSubnetGroup'
    { _mdsgrqDBSubnetGroupDescription :: !(Maybe Text)
    , _mdsgrqDBSubnetGroupName        :: !Text
    , _mdsgrqSubnetIds                :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyDBSubnetGroup' smart constructor.
modifyDBSubnetGroup :: Text -> ModifyDBSubnetGroup
modifyDBSubnetGroup pDBSubnetGroupName_ =
    ModifyDBSubnetGroup'
    { _mdsgrqDBSubnetGroupDescription = Nothing
    , _mdsgrqDBSubnetGroupName = pDBSubnetGroupName_
    , _mdsgrqSubnetIds = mempty
    }

-- | The description for the DB subnet group.
mdsgrqDBSubnetGroupDescription :: Lens' ModifyDBSubnetGroup (Maybe Text)
mdsgrqDBSubnetGroupDescription = lens _mdsgrqDBSubnetGroupDescription (\ s a -> s{_mdsgrqDBSubnetGroupDescription = a});

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens. Must not be \"Default\".
--
-- Example: @mySubnetgroup@
mdsgrqDBSubnetGroupName :: Lens' ModifyDBSubnetGroup Text
mdsgrqDBSubnetGroupName = lens _mdsgrqDBSubnetGroupName (\ s a -> s{_mdsgrqDBSubnetGroupName = a});

-- | The EC2 subnet IDs for the DB subnet group.
mdsgrqSubnetIds :: Lens' ModifyDBSubnetGroup [Text]
mdsgrqSubnetIds = lens _mdsgrqSubnetIds (\ s a -> s{_mdsgrqSubnetIds = a});

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
                 _mdsgrqDBSubnetGroupDescription,
               "DBSubnetGroupName" =: _mdsgrqDBSubnetGroupName,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _mdsgrqSubnetIds]

-- | /See:/ 'modifyDBSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdsgrsDBSubnetGroup'
--
-- * 'mdsgrsStatus'
data ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse'
    { _mdsgrsDBSubnetGroup :: !(Maybe DBSubnetGroup)
    , _mdsgrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyDBSubnetGroupResponse' smart constructor.
modifyDBSubnetGroupResponse :: Int -> ModifyDBSubnetGroupResponse
modifyDBSubnetGroupResponse pStatus_ =
    ModifyDBSubnetGroupResponse'
    { _mdsgrsDBSubnetGroup = Nothing
    , _mdsgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
mdsgrsDBSubnetGroup :: Lens' ModifyDBSubnetGroupResponse (Maybe DBSubnetGroup)
mdsgrsDBSubnetGroup = lens _mdsgrsDBSubnetGroup (\ s a -> s{_mdsgrsDBSubnetGroup = a});

-- | FIXME: Undocumented member.
mdsgrsStatus :: Lens' ModifyDBSubnetGroupResponse Int
mdsgrsStatus = lens _mdsgrsStatus (\ s a -> s{_mdsgrsStatus = a});
