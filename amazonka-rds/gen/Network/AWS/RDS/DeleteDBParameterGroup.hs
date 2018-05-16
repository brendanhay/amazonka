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
-- Module      : Network.AWS.RDS.DeleteDBParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified DBParameterGroup. The DBParameterGroup to be deleted can't be associated with any DB instances.
--
--
module Network.AWS.RDS.DeleteDBParameterGroup
    (
    -- * Creating a Request
      deleteDBParameterGroup
    , DeleteDBParameterGroup
    -- * Request Lenses
    , ddbpgDBParameterGroupName

    -- * Destructuring the Response
    , deleteDBParameterGroupResponse
    , DeleteDBParameterGroupResponse
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
-- /See:/ 'deleteDBParameterGroup' smart constructor.
newtype DeleteDBParameterGroup = DeleteDBParameterGroup'
  { _ddbpgDBParameterGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbpgDBParameterGroupName' - The name of the DB parameter group. Constraints:     * Must be the name of an existing DB parameter group     * You can't delete a default DB parameter group     * Cannot be associated with any DB instances
deleteDBParameterGroup
    :: Text -- ^ 'ddbpgDBParameterGroupName'
    -> DeleteDBParameterGroup
deleteDBParameterGroup pDBParameterGroupName_ =
  DeleteDBParameterGroup' {_ddbpgDBParameterGroupName = pDBParameterGroupName_}


-- | The name of the DB parameter group. Constraints:     * Must be the name of an existing DB parameter group     * You can't delete a default DB parameter group     * Cannot be associated with any DB instances
ddbpgDBParameterGroupName :: Lens' DeleteDBParameterGroup Text
ddbpgDBParameterGroupName = lens _ddbpgDBParameterGroupName (\ s a -> s{_ddbpgDBParameterGroupName = a})

instance AWSRequest DeleteDBParameterGroup where
        type Rs DeleteDBParameterGroup =
             DeleteDBParameterGroupResponse
        request = postQuery rds
        response
          = receiveNull DeleteDBParameterGroupResponse'

instance Hashable DeleteDBParameterGroup where

instance NFData DeleteDBParameterGroup where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBParameterGroupResponse' with the minimum fields required to make a request.
--
deleteDBParameterGroupResponse
    :: DeleteDBParameterGroupResponse
deleteDBParameterGroupResponse = DeleteDBParameterGroupResponse'


instance NFData DeleteDBParameterGroupResponse where
