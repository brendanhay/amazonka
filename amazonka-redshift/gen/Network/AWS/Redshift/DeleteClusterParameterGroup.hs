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
-- Module      : Network.AWS.Redshift.DeleteClusterParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified Amazon Redshift parameter group.
-- You cannot delete a parameter group if it is associated with a cluster.
--
-- /See:/ <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteClusterParameterGroup.html AWS API Reference> for DeleteClusterParameterGroup.
module Network.AWS.Redshift.DeleteClusterParameterGroup
    (
    -- * Creating a Request
      DeleteClusterParameterGroup
    , deleteClusterParameterGroup
    -- * Request Lenses
    , dParameterGroupName

    -- * Destructuring the Response
    , DeleteClusterParameterGroupResponse
    , deleteClusterParameterGroupResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Redshift.Types
import           Network.AWS.Redshift.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'deleteClusterParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dParameterGroupName'
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup'
    { _dParameterGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterParameterGroup' smart constructor.
deleteClusterParameterGroup :: Text -> DeleteClusterParameterGroup
deleteClusterParameterGroup pParameterGroupName_ =
    DeleteClusterParameterGroup'
    { _dParameterGroupName = pParameterGroupName_
    }

-- | The name of the parameter group to be deleted.
--
-- Constraints:
--
-- -   Must be the name of an existing cluster parameter group.
-- -   Cannot delete a default cluster parameter group.
dParameterGroupName :: Lens' DeleteClusterParameterGroup Text
dParameterGroupName = lens _dParameterGroupName (\ s a -> s{_dParameterGroupName = a});

instance AWSRequest DeleteClusterParameterGroup where
        type Sv DeleteClusterParameterGroup = Redshift
        type Rs DeleteClusterParameterGroup =
             DeleteClusterParameterGroupResponse
        request = postQuery
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
               "ParameterGroupName" =: _dParameterGroupName]

-- | /See:/ 'deleteClusterParameterGroupResponse' smart constructor.
data DeleteClusterParameterGroupResponse =
    DeleteClusterParameterGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteClusterParameterGroupResponse' smart constructor.
deleteClusterParameterGroupResponse :: DeleteClusterParameterGroupResponse
deleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'
