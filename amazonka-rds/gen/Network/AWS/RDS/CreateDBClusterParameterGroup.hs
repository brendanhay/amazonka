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
-- Module      : Network.AWS.RDS.CreateDBClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster parameter group.
--
--
-- Parameters in a DB cluster parameter group apply to all of the instances in a DB cluster.
--
-- A DB cluster parameter group is initially created with the default parameters for the database engine used by instances in the DB cluster. To provide custom values for any of the parameters, you must modify the group after creating it using 'ModifyDBClusterParameterGroup' . Once you've created a DB cluster parameter group, you need to associate it with your DB cluster using 'ModifyDBCluster' . When you associate a new DB cluster parameter group with a running DB cluster, you need to reboot the DB instances in the DB cluster without failover for the new DB cluster parameter group and associated settings to take effect.
--
-- /Important:/ After you create a DB cluster parameter group, you should wait at least 5 minutes before creating your first DB cluster that uses that DB cluster parameter group as the default parameter group. This allows Amazon RDS to fully complete the create action before the DB cluster parameter group is used as the default for a new DB cluster. This is especially important for parameters that are critical when creating the default database for a DB cluster, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the 'DescribeDBClusterParameters' command to verify that your DB cluster parameter group has been created or modified.
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.CreateDBClusterParameterGroup
    (
    -- * Creating a Request
      createDBClusterParameterGroup
    , CreateDBClusterParameterGroup
    -- * Request Lenses
    , cdcpgTags
    , cdcpgDBClusterParameterGroupName
    , cdcpgDBParameterGroupFamily
    , cdcpgDescription

    -- * Destructuring the Response
    , createDBClusterParameterGroupResponse
    , CreateDBClusterParameterGroupResponse
    -- * Response Lenses
    , cdbcpgrsDBClusterParameterGroup
    , cdbcpgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createDBClusterParameterGroup' smart constructor.
data CreateDBClusterParameterGroup = CreateDBClusterParameterGroup'
  { _cdcpgTags                        :: !(Maybe [Tag])
  , _cdcpgDBClusterParameterGroupName :: !Text
  , _cdcpgDBParameterGroupFamily      :: !Text
  , _cdcpgDescription                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcpgTags' - Undocumented member.
--
-- * 'cdcpgDBClusterParameterGroupName' - The name of the DB cluster parameter group. Constraints:     * Must match the name of an existing DBClusterParameterGroup.
--
-- * 'cdcpgDBParameterGroupFamily' - The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family. __Aurora MySQL__  Example: @aurora5.6@ , @aurora-mysql5.7@  __Aurora PostgreSQL__  Example: @aurora-postgresql9.6@
--
-- * 'cdcpgDescription' - The description for the DB cluster parameter group.
createDBClusterParameterGroup
    :: Text -- ^ 'cdcpgDBClusterParameterGroupName'
    -> Text -- ^ 'cdcpgDBParameterGroupFamily'
    -> Text -- ^ 'cdcpgDescription'
    -> CreateDBClusterParameterGroup
createDBClusterParameterGroup pDBClusterParameterGroupName_ pDBParameterGroupFamily_ pDescription_ =
  CreateDBClusterParameterGroup'
    { _cdcpgTags = Nothing
    , _cdcpgDBClusterParameterGroupName = pDBClusterParameterGroupName_
    , _cdcpgDBParameterGroupFamily = pDBParameterGroupFamily_
    , _cdcpgDescription = pDescription_
    }


-- | Undocumented member.
cdcpgTags :: Lens' CreateDBClusterParameterGroup [Tag]
cdcpgTags = lens _cdcpgTags (\ s a -> s{_cdcpgTags = a}) . _Default . _Coerce

-- | The name of the DB cluster parameter group. Constraints:     * Must match the name of an existing DBClusterParameterGroup.
cdcpgDBClusterParameterGroupName :: Lens' CreateDBClusterParameterGroup Text
cdcpgDBClusterParameterGroupName = lens _cdcpgDBClusterParameterGroupName (\ s a -> s{_cdcpgDBClusterParameterGroupName = a})

-- | The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family. __Aurora MySQL__  Example: @aurora5.6@ , @aurora-mysql5.7@  __Aurora PostgreSQL__  Example: @aurora-postgresql9.6@
cdcpgDBParameterGroupFamily :: Lens' CreateDBClusterParameterGroup Text
cdcpgDBParameterGroupFamily = lens _cdcpgDBParameterGroupFamily (\ s a -> s{_cdcpgDBParameterGroupFamily = a})

-- | The description for the DB cluster parameter group.
cdcpgDescription :: Lens' CreateDBClusterParameterGroup Text
cdcpgDescription = lens _cdcpgDescription (\ s a -> s{_cdcpgDescription = a})

instance AWSRequest CreateDBClusterParameterGroup
         where
        type Rs CreateDBClusterParameterGroup =
             CreateDBClusterParameterGroupResponse
        request = postQuery rds
        response
          = receiveXMLWrapper
              "CreateDBClusterParameterGroupResult"
              (\ s h x ->
                 CreateDBClusterParameterGroupResponse' <$>
                   (x .@? "DBClusterParameterGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateDBClusterParameterGroup where

instance NFData CreateDBClusterParameterGroup where

instance ToHeaders CreateDBClusterParameterGroup
         where
        toHeaders = const mempty

instance ToPath CreateDBClusterParameterGroup where
        toPath = const "/"

instance ToQuery CreateDBClusterParameterGroup where
        toQuery CreateDBClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateDBClusterParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdcpgTags),
               "DBClusterParameterGroupName" =:
                 _cdcpgDBClusterParameterGroupName,
               "DBParameterGroupFamily" =:
                 _cdcpgDBParameterGroupFamily,
               "Description" =: _cdcpgDescription]

-- | /See:/ 'createDBClusterParameterGroupResponse' smart constructor.
data CreateDBClusterParameterGroupResponse = CreateDBClusterParameterGroupResponse'
  { _cdbcpgrsDBClusterParameterGroup :: !(Maybe DBClusterParameterGroup)
  , _cdbcpgrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBClusterParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbcpgrsDBClusterParameterGroup' - Undocumented member.
--
-- * 'cdbcpgrsResponseStatus' - -- | The response status code.
createDBClusterParameterGroupResponse
    :: Int -- ^ 'cdbcpgrsResponseStatus'
    -> CreateDBClusterParameterGroupResponse
createDBClusterParameterGroupResponse pResponseStatus_ =
  CreateDBClusterParameterGroupResponse'
    { _cdbcpgrsDBClusterParameterGroup = Nothing
    , _cdbcpgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cdbcpgrsDBClusterParameterGroup :: Lens' CreateDBClusterParameterGroupResponse (Maybe DBClusterParameterGroup)
cdbcpgrsDBClusterParameterGroup = lens _cdbcpgrsDBClusterParameterGroup (\ s a -> s{_cdbcpgrsDBClusterParameterGroup = a})

-- | -- | The response status code.
cdbcpgrsResponseStatus :: Lens' CreateDBClusterParameterGroupResponse Int
cdbcpgrsResponseStatus = lens _cdbcpgrsResponseStatus (\ s a -> s{_cdbcpgrsResponseStatus = a})

instance NFData CreateDBClusterParameterGroupResponse
         where
