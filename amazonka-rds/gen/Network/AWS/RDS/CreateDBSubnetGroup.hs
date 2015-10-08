{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB subnet group. DB subnet groups must contain at least
-- one subnet in at least two AZs in the region.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSubnetGroup.html AWS API Reference> for CreateDBSubnetGroup.
module Network.AWS.RDS.CreateDBSubnetGroup
    (
    -- * Creating a Request
      createDBSubnetGroup
    , CreateDBSubnetGroup
    -- * Request Lenses
    , cdbsgTags
    , cdbsgDBSubnetGroupName
    , cdbsgDBSubnetGroupDescription
    , cdbsgSubnetIds

    -- * Destructuring the Response
    , createDBSubnetGroupResponse
    , CreateDBSubnetGroupResponse
    -- * Response Lenses
    , cdsgrsDBSubnetGroup
    , cdsgrsResponseStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBSubnetGroup' smart constructor.
data CreateDBSubnetGroup = CreateDBSubnetGroup'
    { _cdbsgTags                     :: !(Maybe [Tag])
    , _cdbsgDBSubnetGroupName        :: !Text
    , _cdbsgDBSubnetGroupDescription :: !Text
    , _cdbsgSubnetIds                :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbsgTags'
--
-- * 'cdbsgDBSubnetGroupName'
--
-- * 'cdbsgDBSubnetGroupDescription'
--
-- * 'cdbsgSubnetIds'
createDBSubnetGroup
    :: Text -- ^ 'cdbsgDBSubnetGroupName'
    -> Text -- ^ 'cdbsgDBSubnetGroupDescription'
    -> CreateDBSubnetGroup
createDBSubnetGroup pDBSubnetGroupName_ pDBSubnetGroupDescription_ =
    CreateDBSubnetGroup'
    { _cdbsgTags = Nothing
    , _cdbsgDBSubnetGroupName = pDBSubnetGroupName_
    , _cdbsgDBSubnetGroupDescription = pDBSubnetGroupDescription_
    , _cdbsgSubnetIds = mempty
    }

-- | Undocumented member.
cdbsgTags :: Lens' CreateDBSubnetGroup [Tag]
cdbsgTags = lens _cdbsgTags (\ s a -> s{_cdbsgTags = a}) . _Default . _Coerce;

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters,
-- periods, underscores, or hyphens. Must not be default.
--
-- Example: 'mySubnetgroup'
cdbsgDBSubnetGroupName :: Lens' CreateDBSubnetGroup Text
cdbsgDBSubnetGroupName = lens _cdbsgDBSubnetGroupName (\ s a -> s{_cdbsgDBSubnetGroupName = a});

-- | The description for the DB subnet group.
cdbsgDBSubnetGroupDescription :: Lens' CreateDBSubnetGroup Text
cdbsgDBSubnetGroupDescription = lens _cdbsgDBSubnetGroupDescription (\ s a -> s{_cdbsgDBSubnetGroupDescription = a});

-- | The EC2 Subnet IDs for the DB subnet group.
cdbsgSubnetIds :: Lens' CreateDBSubnetGroup [Text]
cdbsgSubnetIds = lens _cdbsgSubnetIds (\ s a -> s{_cdbsgSubnetIds = a}) . _Coerce;

instance AWSRequest CreateDBSubnetGroup where
        type Rs CreateDBSubnetGroup =
             CreateDBSubnetGroupResponse
        request = postQuery rDS
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
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbsgTags),
               "DBSubnetGroupName" =: _cdbsgDBSubnetGroupName,
               "DBSubnetGroupDescription" =:
                 _cdbsgDBSubnetGroupDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _cdbsgSubnetIds]

-- | /See:/ 'createDBSubnetGroupResponse' smart constructor.
data CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse'
    { _cdsgrsDBSubnetGroup  :: !(Maybe DBSubnetGroup)
    , _cdsgrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdsgrsDBSubnetGroup'
--
-- * 'cdsgrsResponseStatus'
createDBSubnetGroupResponse
    :: Int -- ^ 'cdsgrsResponseStatus'
    -> CreateDBSubnetGroupResponse
createDBSubnetGroupResponse pResponseStatus_ =
    CreateDBSubnetGroupResponse'
    { _cdsgrsDBSubnetGroup = Nothing
    , _cdsgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
cdsgrsDBSubnetGroup :: Lens' CreateDBSubnetGroupResponse (Maybe DBSubnetGroup)
cdsgrsDBSubnetGroup = lens _cdsgrsDBSubnetGroup (\ s a -> s{_cdsgrsDBSubnetGroup = a});

-- | The response status code.
cdsgrsResponseStatus :: Lens' CreateDBSubnetGroupResponse Int
cdsgrsResponseStatus = lens _cdsgrsResponseStatus (\ s a -> s{_cdsgrsResponseStatus = a});
