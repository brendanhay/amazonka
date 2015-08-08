{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB cluster parameter group. The DB cluster parameter
-- group to be deleted cannot be associated with any DB clusters.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBClusterParameterGroup.html AWS API Reference> for DeleteDBClusterParameterGroup.
module Network.AWS.RDS.DeleteDBClusterParameterGroup
    (
    -- * Creating a Request
      DeleteDBClusterParameterGroup
    , deleteDBClusterParameterGroup
    -- * Request Lenses
    , ddbcpgDBClusterParameterGroupName

    -- * Destructuring the Response
    , DeleteDBClusterParameterGroupResponse
    , deleteDBClusterParameterGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbcpgDBClusterParameterGroupName'
newtype DeleteDBClusterParameterGroup = DeleteDBClusterParameterGroup'
    { _ddbcpgDBClusterParameterGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBClusterParameterGroup' smart constructor.
deleteDBClusterParameterGroup :: Text -> DeleteDBClusterParameterGroup
deleteDBClusterParameterGroup pDBClusterParameterGroupName_ =
    DeleteDBClusterParameterGroup'
    { _ddbcpgDBClusterParameterGroupName = pDBClusterParameterGroupName_
    }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB cluster parameter group.
-- -   You cannot delete a default DB cluster parameter group.
-- -   Cannot be associated with any DB clusters.
ddbcpgDBClusterParameterGroupName :: Lens' DeleteDBClusterParameterGroup Text
ddbcpgDBClusterParameterGroupName = lens _ddbcpgDBClusterParameterGroupName (\ s a -> s{_ddbcpgDBClusterParameterGroupName = a});

instance AWSRequest DeleteDBClusterParameterGroup
         where
        type Sv DeleteDBClusterParameterGroup = RDS
        type Rs DeleteDBClusterParameterGroup =
             DeleteDBClusterParameterGroupResponse
        request = postQuery
        response
          = receiveNull DeleteDBClusterParameterGroupResponse'

instance ToHeaders DeleteDBClusterParameterGroup
         where
        toHeaders = const mempty

instance ToPath DeleteDBClusterParameterGroup where
        toPath = const "/"

instance ToQuery DeleteDBClusterParameterGroup where
        toQuery DeleteDBClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteDBClusterParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterParameterGroupName" =:
                 _ddbcpgDBClusterParameterGroupName]

-- | /See:/ 'deleteDBClusterParameterGroupResponse' smart constructor.
data DeleteDBClusterParameterGroupResponse =
    DeleteDBClusterParameterGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBClusterParameterGroupResponse' smart constructor.
deleteDBClusterParameterGroupResponse :: DeleteDBClusterParameterGroupResponse
deleteDBClusterParameterGroupResponse = DeleteDBClusterParameterGroupResponse'
