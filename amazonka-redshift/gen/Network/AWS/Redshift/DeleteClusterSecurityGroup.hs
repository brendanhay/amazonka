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
-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift security group.
--
--
-- For information about managing security groups, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups> in the /Amazon Redshift Cluster Management Guide/ .
--
module Network.AWS.Redshift.DeleteClusterSecurityGroup
    (
    -- * Creating a Request
      deleteClusterSecurityGroup
    , DeleteClusterSecurityGroup
    -- * Request Lenses
    , dClusterSecurityGroupName

    -- * Destructuring the Response
    , deleteClusterSecurityGroupResponse
    , DeleteClusterSecurityGroupResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteClusterSecurityGroup' smart constructor.
newtype DeleteClusterSecurityGroup = DeleteClusterSecurityGroup'
  { _dClusterSecurityGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dClusterSecurityGroupName' - The name of the cluster security group to be deleted.
deleteClusterSecurityGroup
    :: Text -- ^ 'dClusterSecurityGroupName'
    -> DeleteClusterSecurityGroup
deleteClusterSecurityGroup pClusterSecurityGroupName_ =
  DeleteClusterSecurityGroup'
    {_dClusterSecurityGroupName = pClusterSecurityGroupName_}


-- | The name of the cluster security group to be deleted.
dClusterSecurityGroupName :: Lens' DeleteClusterSecurityGroup Text
dClusterSecurityGroupName = lens _dClusterSecurityGroupName (\ s a -> s{_dClusterSecurityGroupName = a})

instance AWSRequest DeleteClusterSecurityGroup where
        type Rs DeleteClusterSecurityGroup =
             DeleteClusterSecurityGroupResponse
        request = postQuery redshift
        response
          = receiveNull DeleteClusterSecurityGroupResponse'

instance Hashable DeleteClusterSecurityGroup where

instance NFData DeleteClusterSecurityGroup where

instance ToHeaders DeleteClusterSecurityGroup where
        toHeaders = const mempty

instance ToPath DeleteClusterSecurityGroup where
        toPath = const "/"

instance ToQuery DeleteClusterSecurityGroup where
        toQuery DeleteClusterSecurityGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteClusterSecurityGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterSecurityGroupName" =:
                 _dClusterSecurityGroupName]

-- | /See:/ 'deleteClusterSecurityGroupResponse' smart constructor.
data DeleteClusterSecurityGroupResponse =
  DeleteClusterSecurityGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterSecurityGroupResponse' with the minimum fields required to make a request.
--
deleteClusterSecurityGroupResponse
    :: DeleteClusterSecurityGroupResponse
deleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse'


instance NFData DeleteClusterSecurityGroupResponse
         where
