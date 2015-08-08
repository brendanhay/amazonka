{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DBParameterGroup. The DBParameterGroup to be deleted
-- cannot be associated with any DB instances.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBParameterGroup.html AWS API Reference> for DeleteDBParameterGroup.
module Network.AWS.RDS.DeleteDBParameterGroup
    (
    -- * Creating a Request
      DeleteDBParameterGroup
    , deleteDBParameterGroup
    -- * Request Lenses
    , ddbpgDBParameterGroupName

    -- * Destructuring the Response
    , DeleteDBParameterGroupResponse
    , deleteDBParameterGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteDBParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbpgDBParameterGroupName'
newtype DeleteDBParameterGroup = DeleteDBParameterGroup'
    { _ddbpgDBParameterGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBParameterGroup' smart constructor.
deleteDBParameterGroup :: Text -> DeleteDBParameterGroup
deleteDBParameterGroup pDBParameterGroupName_ =
    DeleteDBParameterGroup'
    { _ddbpgDBParameterGroupName = pDBParameterGroupName_
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB parameter group
-- -   You cannot delete a default DB parameter group
-- -   Cannot be associated with any DB instances
ddbpgDBParameterGroupName :: Lens' DeleteDBParameterGroup Text
ddbpgDBParameterGroupName = lens _ddbpgDBParameterGroupName (\ s a -> s{_ddbpgDBParameterGroupName = a});

instance AWSRequest DeleteDBParameterGroup where
        type Sv DeleteDBParameterGroup = RDS
        type Rs DeleteDBParameterGroup =
             DeleteDBParameterGroupResponse
        request = postQuery
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
               "DBParameterGroupName" =: _ddbpgDBParameterGroupName]

-- | /See:/ 'deleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse =
    DeleteDBParameterGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBParameterGroupResponse' smart constructor.
deleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse
deleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
