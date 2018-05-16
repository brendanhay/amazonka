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
-- Module      : Network.AWS.RDS.DeleteDBClusterParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DB cluster parameter group. The DB cluster parameter group to be deleted can't be associated with any DB clusters.
--
--
-- For more information on Amazon Aurora, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS> in the /Amazon RDS User Guide./
--
module Network.AWS.RDS.DeleteDBClusterParameterGroup
    (
    -- * Creating a Request
      deleteDBClusterParameterGroup
    , DeleteDBClusterParameterGroup
    -- * Request Lenses
    , ddbcpgDBClusterParameterGroupName

    -- * Destructuring the Response
    , deleteDBClusterParameterGroupResponse
    , DeleteDBClusterParameterGroupResponse
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
-- /See:/ 'deleteDBClusterParameterGroup' smart constructor.
newtype DeleteDBClusterParameterGroup = DeleteDBClusterParameterGroup'
  { _ddbcpgDBClusterParameterGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbcpgDBClusterParameterGroupName' - The name of the DB cluster parameter group. Constraints:     * Must be the name of an existing DB cluster parameter group.     * You can't delete a default DB cluster parameter group.     * Cannot be associated with any DB clusters.
deleteDBClusterParameterGroup
    :: Text -- ^ 'ddbcpgDBClusterParameterGroupName'
    -> DeleteDBClusterParameterGroup
deleteDBClusterParameterGroup pDBClusterParameterGroupName_ =
  DeleteDBClusterParameterGroup'
    {_ddbcpgDBClusterParameterGroupName = pDBClusterParameterGroupName_}


-- | The name of the DB cluster parameter group. Constraints:     * Must be the name of an existing DB cluster parameter group.     * You can't delete a default DB cluster parameter group.     * Cannot be associated with any DB clusters.
ddbcpgDBClusterParameterGroupName :: Lens' DeleteDBClusterParameterGroup Text
ddbcpgDBClusterParameterGroupName = lens _ddbcpgDBClusterParameterGroupName (\ s a -> s{_ddbcpgDBClusterParameterGroupName = a})

instance AWSRequest DeleteDBClusterParameterGroup
         where
        type Rs DeleteDBClusterParameterGroup =
             DeleteDBClusterParameterGroupResponse
        request = postQuery rds
        response
          = receiveNull DeleteDBClusterParameterGroupResponse'

instance Hashable DeleteDBClusterParameterGroup where

instance NFData DeleteDBClusterParameterGroup where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBClusterParameterGroupResponse' with the minimum fields required to make a request.
--
deleteDBClusterParameterGroupResponse
    :: DeleteDBClusterParameterGroupResponse
deleteDBClusterParameterGroupResponse = DeleteDBClusterParameterGroupResponse'


instance NFData DeleteDBClusterParameterGroupResponse
         where
