{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Redshift security group.
--
-- You cannot delete a security group that is associated with any clusters.
-- You cannot delete the default security group.
--
-- For information about managing security groups, go to
-- <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-security-groups.html Amazon Redshift Cluster Security Groups>
-- in the /Amazon Redshift Cluster Management Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterSecurityGroup.html AWS API Reference> for DeleteClusterSecurityGroup.
module Network.AWS.Redshift.DeleteClusterSecurityGroup
    (
    -- * Creating a Request
      DeleteClusterSecurityGroup
    , deleteClusterSecurityGroup
    -- * Request Lenses
    , dClusterSecurityGroupName

    -- * Destructuring the Response
    , DeleteClusterSecurityGroupResponse
    , deleteClusterSecurityGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteClusterSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dClusterSecurityGroupName'
newtype DeleteClusterSecurityGroup = DeleteClusterSecurityGroup'
    { _dClusterSecurityGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterSecurityGroup' smart constructor.
deleteClusterSecurityGroup :: Text -> DeleteClusterSecurityGroup
deleteClusterSecurityGroup pClusterSecurityGroupName_ =
    DeleteClusterSecurityGroup'
    { _dClusterSecurityGroupName = pClusterSecurityGroupName_
    }

-- | The name of the cluster security group to be deleted.
dClusterSecurityGroupName :: Lens' DeleteClusterSecurityGroup Text
dClusterSecurityGroupName = lens _dClusterSecurityGroupName (\ s a -> s{_dClusterSecurityGroupName = a});

instance AWSRequest DeleteClusterSecurityGroup where
        type Sv DeleteClusterSecurityGroup = Redshift
        type Rs DeleteClusterSecurityGroup =
             DeleteClusterSecurityGroupResponse
        request = postQuery
        response
          = receiveNull DeleteClusterSecurityGroupResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterSecurityGroupResponse' smart constructor.
deleteClusterSecurityGroupResponse :: DeleteClusterSecurityGroupResponse
deleteClusterSecurityGroupResponse = DeleteClusterSecurityGroupResponse'
