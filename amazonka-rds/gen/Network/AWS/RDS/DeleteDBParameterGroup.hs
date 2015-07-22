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
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DBParameterGroup. The DBParameterGroup to be deleted
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
    , ddbpgrqDBParameterGroupName

    -- * Response
    , DeleteDBParameterGroupResponse
    -- ** Response constructor
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
-- * 'ddbpgrqDBParameterGroupName'
newtype DeleteDBParameterGroup = DeleteDBParameterGroup'
    { _ddbpgrqDBParameterGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBParameterGroup' smart constructor.
deleteDBParameterGroup :: Text -> DeleteDBParameterGroup
deleteDBParameterGroup pDBParameterGroupName =
    DeleteDBParameterGroup'
    { _ddbpgrqDBParameterGroupName = pDBParameterGroupName
    }

-- | The name of the DB parameter group.
--
-- Constraints:
--
-- -   Must be the name of an existing DB parameter group
-- -   You cannot delete a default DB parameter group
-- -   Cannot be associated with any DB instances
ddbpgrqDBParameterGroupName :: Lens' DeleteDBParameterGroup Text
ddbpgrqDBParameterGroupName = lens _ddbpgrqDBParameterGroupName (\ s a -> s{_ddbpgrqDBParameterGroupName = a});

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
               "DBParameterGroupName" =:
                 _ddbpgrqDBParameterGroupName]

-- | /See:/ 'deleteDBParameterGroupResponse' smart constructor.
data DeleteDBParameterGroupResponse =
    DeleteDBParameterGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDBParameterGroupResponse' smart constructor.
deleteDBParameterGroupResponse :: DeleteDBParameterGroupResponse
deleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'
