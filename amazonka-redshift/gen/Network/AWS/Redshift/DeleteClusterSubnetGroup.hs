{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified cluster subnet group.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterSubnetGroup.html>
module Network.AWS.Redshift.DeleteClusterSubnetGroup
    (
    -- * Request
      DeleteClusterSubnetGroup
    -- ** Request constructor
    , deleteClusterSubnetGroup
    -- ** Request lenses
    , dcsgClusterSubnetGroupName

    -- * Response
    , DeleteClusterSubnetGroupResponse
    -- ** Response constructor
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
deleteClusterSubnetGroup pClusterSubnetGroupName =
    DeleteClusterSubnetGroup'
    { _dcsgClusterSubnetGroupName = pClusterSubnetGroupName
    }

-- | The name of the cluster subnet group name to be deleted.
dcsgClusterSubnetGroupName :: Lens' DeleteClusterSubnetGroup Text
dcsgClusterSubnetGroupName = lens _dcsgClusterSubnetGroupName (\ s a -> s{_dcsgClusterSubnetGroupName = a});

instance AWSRequest DeleteClusterSubnetGroup where
        type Sv DeleteClusterSubnetGroup = Redshift
        type Rs DeleteClusterSubnetGroup =
             DeleteClusterSubnetGroupResponse
        request = post
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
