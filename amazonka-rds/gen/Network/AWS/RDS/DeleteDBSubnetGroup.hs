{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
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

-- | Deletes a DB subnet group.
--
-- The specified database subnet group must not be associated with any DB
-- instances.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBSubnetGroup.html>
module Network.AWS.RDS.DeleteDBSubnetGroup
    (
    -- * Request
      DeleteDBSubnetGroup
    -- ** Request constructor
    , deleteDBSubnetGroup
    -- ** Request lenses
    , delDBSubnetGroupName

    -- * Response
    , DeleteDBSubnetGroupResponse
    -- ** Response constructor
    , deleteDBSubnetGroupResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'deleteDBSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delDBSubnetGroupName'
newtype DeleteDBSubnetGroup = DeleteDBSubnetGroup'{_delDBSubnetGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDBSubnetGroup' smart constructor.
deleteDBSubnetGroup :: Text -> DeleteDBSubnetGroup
deleteDBSubnetGroup pDBSubnetGroupName = DeleteDBSubnetGroup'{_delDBSubnetGroupName = pDBSubnetGroupName};

-- | The name of the database subnet group to delete.
--
-- You cannot delete the default subnet group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
delDBSubnetGroupName :: Lens' DeleteDBSubnetGroup Text
delDBSubnetGroupName = lens _delDBSubnetGroupName (\ s a -> s{_delDBSubnetGroupName = a});

instance AWSRequest DeleteDBSubnetGroup where
        type Sv DeleteDBSubnetGroup = RDS
        type Rs DeleteDBSubnetGroup =
             DeleteDBSubnetGroupResponse
        request = post
        response = receiveNull DeleteDBSubnetGroupResponse'

instance ToHeaders DeleteDBSubnetGroup where
        toHeaders = const mempty

instance ToPath DeleteDBSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteDBSubnetGroup where
        toQuery DeleteDBSubnetGroup'{..}
          = mconcat
              ["Action" =: ("DeleteDBSubnetGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSubnetGroupName" =: _delDBSubnetGroupName]

-- | /See:/ 'deleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse' deriving (Eq, Read, Show)

-- | 'DeleteDBSubnetGroupResponse' smart constructor.
deleteDBSubnetGroupResponse :: DeleteDBSubnetGroupResponse
deleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse';
