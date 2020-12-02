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
-- Module      : Network.AWS.RDS.DeleteDBSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DB subnet group.
--
--
module Network.AWS.RDS.DeleteDBSubnetGroup
    (
    -- * Creating a Request
      deleteDBSubnetGroup
    , DeleteDBSubnetGroup
    -- * Request Lenses
    , ddbsgDBSubnetGroupName

    -- * Destructuring the Response
    , deleteDBSubnetGroupResponse
    , DeleteDBSubnetGroupResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteDBSubnetGroup' smart constructor.
newtype DeleteDBSubnetGroup = DeleteDBSubnetGroup'
  { _ddbsgDBSubnetGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbsgDBSubnetGroupName' - The name of the database subnet group to delete. Constraints: Constraints: Must match the name of an existing DBSubnetGroup. Must not be default. Example: @mySubnetgroup@
deleteDBSubnetGroup
    :: Text -- ^ 'ddbsgDBSubnetGroupName'
    -> DeleteDBSubnetGroup
deleteDBSubnetGroup pDBSubnetGroupName_ =
  DeleteDBSubnetGroup' {_ddbsgDBSubnetGroupName = pDBSubnetGroupName_}


-- | The name of the database subnet group to delete. Constraints: Constraints: Must match the name of an existing DBSubnetGroup. Must not be default. Example: @mySubnetgroup@
ddbsgDBSubnetGroupName :: Lens' DeleteDBSubnetGroup Text
ddbsgDBSubnetGroupName = lens _ddbsgDBSubnetGroupName (\ s a -> s{_ddbsgDBSubnetGroupName = a})

instance AWSRequest DeleteDBSubnetGroup where
        type Rs DeleteDBSubnetGroup =
             DeleteDBSubnetGroupResponse
        request = postQuery rds
        response = receiveNull DeleteDBSubnetGroupResponse'

instance Hashable DeleteDBSubnetGroup where

instance NFData DeleteDBSubnetGroup where

instance ToHeaders DeleteDBSubnetGroup where
        toHeaders = const mempty

instance ToPath DeleteDBSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteDBSubnetGroup where
        toQuery DeleteDBSubnetGroup'{..}
          = mconcat
              ["Action" =: ("DeleteDBSubnetGroup" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSubnetGroupName" =: _ddbsgDBSubnetGroupName]

-- | /See:/ 'deleteDBSubnetGroupResponse' smart constructor.
data DeleteDBSubnetGroupResponse =
  DeleteDBSubnetGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBSubnetGroupResponse' with the minimum fields required to make a request.
--
deleteDBSubnetGroupResponse
    :: DeleteDBSubnetGroupResponse
deleteDBSubnetGroupResponse = DeleteDBSubnetGroupResponse'


instance NFData DeleteDBSubnetGroupResponse where
