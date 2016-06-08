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
-- Module      : Network.AWS.RDS.DeleteDBSecurityGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB security group.
--
-- The specified DB security group must not be associated with any DB instances.
module Network.AWS.RDS.DeleteDBSecurityGroup
    (
    -- * Creating a Request
      deleteDBSecurityGroup
    , DeleteDBSecurityGroup
    -- * Request Lenses
    , ddsgDBSecurityGroupName

    -- * Destructuring the Response
    , deleteDBSecurityGroupResponse
    , DeleteDBSecurityGroupResponse
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBSecurityGroup' smart constructor.
newtype DeleteDBSecurityGroup = DeleteDBSecurityGroup'
    { _ddsgDBSecurityGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteDBSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddsgDBSecurityGroupName'
deleteDBSecurityGroup
    :: Text -- ^ 'ddsgDBSecurityGroupName'
    -> DeleteDBSecurityGroup
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
-- -   Cannot contain spaces
ddsgDBSecurityGroupName :: Lens' DeleteDBSecurityGroup Text
ddsgDBSecurityGroupName = lens _ddsgDBSecurityGroupName (\ s a -> s{_ddsgDBSecurityGroupName = a});

instance AWSRequest DeleteDBSecurityGroup where
        type Rs DeleteDBSecurityGroup =
             DeleteDBSecurityGroupResponse
        request = postQuery rds
        response = receiveNull DeleteDBSecurityGroupResponse'

instance Hashable DeleteDBSecurityGroup

instance NFData DeleteDBSecurityGroup

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

-- | Creates a value of 'DeleteDBSecurityGroupResponse' with the minimum fields required to make a request.
--
deleteDBSecurityGroupResponse
    :: DeleteDBSecurityGroupResponse
deleteDBSecurityGroupResponse = DeleteDBSecurityGroupResponse'

instance NFData DeleteDBSecurityGroupResponse
