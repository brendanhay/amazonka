{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB security group.
--
-- The specified DB security group must not be associated with any DB
-- instances.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBSecurityGroup.html>
module Network.AWS.RDS.DeleteDBSecurityGroup
    (
    -- * Request
      DeleteDBSecurityGroup
    -- ** Request constructor
    , deleteDBSecurityGroup
    -- ** Request lenses
    , ddsgDBSecurityGroupName

    -- * Response
    , DeleteDBSecurityGroupResponse
    -- ** Response constructor
    , deleteDBSecurityGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddsgDBSecurityGroupName'
newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup'
    { _ddsgDBSecurityGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSecurityGroup' smart constructor.
deleteDBSecurityGroup :: Text -> DeleteDBSecurityGroup
deleteDBSecurityGroup pDBSecurityGroupName_ =
    DeleteDBSecurityGroup'
    { _ddsgDBSecurityGroupName = pDBSecurityGroupName_
    }

-- | The name of the DB security group to delete.
--
-- You cannot delete the default DB security group.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
-- -   Must not be \"Default\"
-- -   May not contain spaces
ddsgDBSecurityGroupName :: Lens' DeleteDBSecurityGroup Text
ddsgDBSecurityGroupName = lens _ddsgDBSecurityGroupName (\ s a -> s{_ddsgDBSecurityGroupName = a});

instance AWSRequest DeleteDBSecurityGroup where
        type Sv DeleteDBSecurityGroup = RDS
        type Rs DeleteDBSecurityGroup =
             DeleteDBSecurityGroupResponse
        request = postQuery
        response = receiveNull DeleteDBSecurityGroupResponse'

instance ToHeaders DeleteDBSecurityGroup where
        toHeaders = const mempty

instance ToPath DeleteDBSecurityGroup where
        toPath = const "/"

instance ToQuery DeleteDBSecurityGroup where
        toQuery DeleteDBSecurityGroup'{..}
          = mconcat
              ["Action" =: ("DeleteDBSecurityGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSecurityGroupName" =: _ddsgDBSecurityGroupName]

-- | /See:/ 'deleteDBSecurityGroupResponse' smart constructor.
data DeleteDBSecurityGroupResponse =
    DeleteDBSecurityGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBSecurityGroupResponse' smart constructor.
deleteDBSecurityGroupResponse :: DeleteDBSecurityGroupResponse
deleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'
