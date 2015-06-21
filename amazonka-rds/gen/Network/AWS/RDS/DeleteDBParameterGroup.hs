{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
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

-- | Deletes a specified DBParameterGroup. The DBParameterGroup to be deleted
-- cannot be associated with any DB instances.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBParameterGroup.html>
module Network.AWS.RDS.DeleteDBParameterGroup
    (
    -- * Request
      DeleteDBParameterGroup
    -- ** Request constructor
    , deleteDBParameterGroup
    -- ** Request lenses
    , delDBParameterGroupName

    -- * Response
    , DeleteDBParameterGroupResponse
    -- ** Response constructor
    , deleteDBParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delDBParameterGroupName'
newtype DeleteDBParameterGroup = DeleteDBParameterGroup'{_delDBParameterGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteDBParameterGroup' smart constructor.
deleteDBParameterGroup :: Text -> DeleteDBParameterGroup
deleteDBParameterGroup pDBParameterGroupName = DeleteDBParameterGroup'{_delDBParameterGroupName = pDBParameterGroupName};

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB parameter group
-- -   You cannot delete a default DB parameter group
-- -   Cannot be associated with any DB instances
delDBParameterGroupName :: Lens' DeleteDBParameterGroup Text
delDBParameterGroupName = lens _delDBParameterGroupName (\ s a -> s{_delDBParameterGroupName = a});

instance AWSRequest DeleteDBParameterGroup where
        type Sv DeleteDBParameterGroup = RDS
        type Rs DeleteDBParameterGroup =
             DeleteDBParameterGroupResponse
        request = post
        response
          = receiveNull DeleteDBParameterGroupResponse'

instance ToHeaders DeleteDBParameterGroup where
        toHeaders = const mempty

instance ToPath DeleteDBParameterGroup where
        toPath = const "/"

instance ToQuery DeleteDBParameterGroup where
        toQuery DeleteDBParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteDBParameterGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBParameterGroupName" =: _delDBParameterGroupName]

-- | /See:/ 'deleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse = DeleteDBParameterGroupResponse' deriving (Eq, Read, Show)

-- | 'DeleteDBParameterGroupResponse' smart constructor.
deleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse
deleteDBParameterGroupResponse = DeleteDBParameterGroupResponse';
