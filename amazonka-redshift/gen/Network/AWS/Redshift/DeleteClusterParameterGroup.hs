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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified Amazon Redshift parameter group.
--
--
module Network.AWS.Redshift.DeleteClusterParameterGroup
    (
    -- * Creating a Request
      deleteClusterParameterGroup
    , DeleteClusterParameterGroup
    -- * Request Lenses
    , dParameterGroupName

    -- * Destructuring the Response
    , deleteClusterParameterGroupResponse
    , DeleteClusterParameterGroupResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteClusterParameterGroup' smart constructor.
newtype DeleteClusterParameterGroup = DeleteClusterParameterGroup'
  { _dParameterGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dParameterGroupName' - The name of the parameter group to be deleted. Constraints:     * Must be the name of an existing cluster parameter group.     * Cannot delete a default cluster parameter group.
deleteClusterParameterGroup
    :: Text -- ^ 'dParameterGroupName'
    -> DeleteClusterParameterGroup
deleteClusterParameterGroup pParameterGroupName_ =
  DeleteClusterParameterGroup' {_dParameterGroupName = pParameterGroupName_}


-- | The name of the parameter group to be deleted. Constraints:     * Must be the name of an existing cluster parameter group.     * Cannot delete a default cluster parameter group.
dParameterGroupName :: Lens' DeleteClusterParameterGroup Text
dParameterGroupName = lens _dParameterGroupName (\ s a -> s{_dParameterGroupName = a})

instance AWSRequest DeleteClusterParameterGroup where
        type Rs DeleteClusterParameterGroup =
             DeleteClusterParameterGroupResponse
        request = postQuery redshift
        response
          = receiveNull DeleteClusterParameterGroupResponse'

instance Hashable DeleteClusterParameterGroup where

instance NFData DeleteClusterParameterGroup where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterParameterGroupResponse' with the minimum fields required to make a request.
--
deleteClusterParameterGroupResponse
    :: DeleteClusterParameterGroupResponse
deleteClusterParameterGroupResponse = DeleteClusterParameterGroupResponse'


instance NFData DeleteClusterParameterGroupResponse
         where
