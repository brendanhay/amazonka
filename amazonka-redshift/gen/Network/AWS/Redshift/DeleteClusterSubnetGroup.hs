{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster subnet group.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterSubnetGroup.html AWS API Reference> for DeleteClusterSubnetGroup.
module Network.AWS.Redshift.DeleteClusterSubnetGroup
    (
    -- * Creating a Request
      DeleteClusterSubnetGroup
    , deleteClusterSubnetGroup
    -- * Request Lenses
    , dcsgClusterSubnetGroupName

    -- * Destructuring the Response
    , DeleteClusterSubnetGroupResponse
    , deleteClusterSubnetGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteClusterSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgClusterSubnetGroupName'
newtype DeleteClusterSubnetGroup = DeleteClusterSubnetGroup'
    { _dcsgClusterSubnetGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterSubnetGroup' smart constructor.
deleteClusterSubnetGroup :: Text -> DeleteClusterSubnetGroup
deleteClusterSubnetGroup pClusterSubnetGroupName_ =
    DeleteClusterSubnetGroup'
    { _dcsgClusterSubnetGroupName = pClusterSubnetGroupName_
    }

-- | The name of the cluster subnet group name to be deleted.
dcsgClusterSubnetGroupName :: Lens' DeleteClusterSubnetGroup Text
dcsgClusterSubnetGroupName = lens _dcsgClusterSubnetGroupName (\ s a -> s{_dcsgClusterSubnetGroupName = a});

instance AWSRequest DeleteClusterSubnetGroup where
        type Sv DeleteClusterSubnetGroup = Redshift
        type Rs DeleteClusterSubnetGroup =
             DeleteClusterSubnetGroupResponse
        request = postQuery
        response
          = receiveNull DeleteClusterSubnetGroupResponse'

instance ToHeaders DeleteClusterSubnetGroup where
        toHeaders = const mempty

instance ToPath DeleteClusterSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteClusterSubnetGroup where
        toQuery DeleteClusterSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteClusterSubnetGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterSubnetGroupName" =:
                 _dcsgClusterSubnetGroupName]

-- | /See:/ 'deleteClusterSubnetGroupResponse' smart constructor.
data DeleteClusterSubnetGroupResponse =
    DeleteClusterSubnetGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterSubnetGroupResponse' smart constructor.
deleteClusterSubnetGroupResponse :: DeleteClusterSubnetGroupResponse
deleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse'
