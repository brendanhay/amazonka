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
-- Module      : Network.AWS.Redshift.DeleteClusterSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cluster subnet group.
--
--
module Network.AWS.Redshift.DeleteClusterSubnetGroup
    (
    -- * Creating a Request
      deleteClusterSubnetGroup
    , DeleteClusterSubnetGroup
    -- * Request Lenses
    , dcsgClusterSubnetGroupName

    -- * Destructuring the Response
    , deleteClusterSubnetGroupResponse
    , DeleteClusterSubnetGroupResponse
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
-- /See:/ 'deleteClusterSubnetGroup' smart constructor.
newtype DeleteClusterSubnetGroup = DeleteClusterSubnetGroup'
  { _dcsgClusterSubnetGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgClusterSubnetGroupName' - The name of the cluster subnet group name to be deleted.
deleteClusterSubnetGroup
    :: Text -- ^ 'dcsgClusterSubnetGroupName'
    -> DeleteClusterSubnetGroup
deleteClusterSubnetGroup pClusterSubnetGroupName_ =
  DeleteClusterSubnetGroup'
    {_dcsgClusterSubnetGroupName = pClusterSubnetGroupName_}


-- | The name of the cluster subnet group name to be deleted.
dcsgClusterSubnetGroupName :: Lens' DeleteClusterSubnetGroup Text
dcsgClusterSubnetGroupName = lens _dcsgClusterSubnetGroupName (\ s a -> s{_dcsgClusterSubnetGroupName = a})

instance AWSRequest DeleteClusterSubnetGroup where
        type Rs DeleteClusterSubnetGroup =
             DeleteClusterSubnetGroupResponse
        request = postQuery redshift
        response
          = receiveNull DeleteClusterSubnetGroupResponse'

instance Hashable DeleteClusterSubnetGroup where

instance NFData DeleteClusterSubnetGroup where

instance ToHeaders DeleteClusterSubnetGroup where
        toHeaders = const mempty

instance ToPath DeleteClusterSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteClusterSubnetGroup where
        toQuery DeleteClusterSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteClusterSubnetGroup" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterSubnetGroupName" =:
                 _dcsgClusterSubnetGroupName]

-- | /See:/ 'deleteClusterSubnetGroupResponse' smart constructor.
data DeleteClusterSubnetGroupResponse =
  DeleteClusterSubnetGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteClusterSubnetGroupResponse' with the minimum fields required to make a request.
--
deleteClusterSubnetGroupResponse
    :: DeleteClusterSubnetGroupResponse
deleteClusterSubnetGroupResponse = DeleteClusterSubnetGroupResponse'


instance NFData DeleteClusterSubnetGroupResponse
         where
