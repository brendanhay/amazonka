{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.DeleteClusterParameterGroup
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

-- | Deletes a specified Amazon Redshift parameter group.
-- You cannot delete a parameter group if it is associated with a cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterParameterGroup.html>
module Network.AWS.Redshift.DeleteClusterParameterGroup
    (
    -- * Request
      DeleteClusterParameterGroup
    -- ** Request constructor
    , deleteClusterParameterGroup
    -- ** Request lenses
    , delParameterGroupName

    -- * Response
    , DeleteClusterParameterGroupResponse
    -- ** Response constructor
    , deleteClusterParameterGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delParameterGroupName'
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup'
    { _delParameterGroupName :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteClusterParameterGroup' smart constructor.
deleteClusterParameterGroup :: Text -> DeleteClusterParameterGroup
deleteClusterParameterGroup pParameterGroupName =
    DeleteClusterParameterGroup'
    { _delParameterGroupName = pParameterGroupName
    }

-- | The name of the parameter group to be deleted.
--
-- Constraints:
--
-- -   Must be the name of an existing cluster parameter group.
-- -   Cannot delete a default cluster parameter group.
delParameterGroupName :: Lens' DeleteClusterParameterGroup Text
delParameterGroupName = lens _delParameterGroupName (\ s a -> s{_delParameterGroupName = a});

instance AWSRequest DeleteClusterParameterGroup where
        type Sv DeleteClusterParameterGroup = Redshift
        type Rs DeleteClusterParameterGroup =
             DeleteClusterParameterGroupResponse
        request = post
        response
          = receiveNull DeleteClusterParameterGroupResponse'

instance ToHeaders DeleteClusterParameterGroup where
        toHeaders = const mempty

instance ToPath DeleteClusterParameterGroup where
        toPath = const "/"

instance ToQuery DeleteClusterParameterGroup where
        toQuery DeleteClusterParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteClusterParameterGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ParameterGroupName" =: _delParameterGroupName]

-- | /See:/ 'deleteClusterParameterGroupResponse' smart constructor.
data DeleteClusterParameterGroupResponse =
    DeleteClusterParameterGroupResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteClusterParameterGroupResponse' smart constructor.
deleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse
deleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'
