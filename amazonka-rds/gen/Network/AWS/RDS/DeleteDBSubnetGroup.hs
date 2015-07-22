{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB subnet group.
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
    , ddbsgrqDBSubnetGroupName

    -- * Response
    , DeleteDBSubnetGroupResponse
    -- ** Response constructor
    , deleteDBSubnetGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsgrqDBSubnetGroupName'
newtype DeleteDBSubnetGroup = DeleteDBSubnetGroup'
    { _ddbsgrqDBSubnetGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSubnetGroup' smart constructor.
deleteDBSubnetGroup :: Text -> DeleteDBSubnetGroup
deleteDBSubnetGroup pDBSubnetGroupName =
    DeleteDBSubnetGroup'
    { _ddbsgrqDBSubnetGroupName = pDBSubnetGroupName
    }

-- | The name of the database subnet group to delete.
--
-- You cannot delete the default subnet group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
ddbsgrqDBSubnetGroupName :: Lens' DeleteDBSubnetGroup Text
ddbsgrqDBSubnetGroupName = lens _ddbsgrqDBSubnetGroupName (\ s a -> s{_ddbsgrqDBSubnetGroupName = a});

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
               "DBSubnetGroupName" =: _ddbsgrqDBSubnetGroupName]

-- | /See:/ 'deleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse =
    DeleteDBSubnetGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSubnetGroupResponse' smart constructor.
deleteDBSubnetGroupResponse :: DeleteDBSubnetGroupResponse
deleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'
