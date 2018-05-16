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
-- Module      : Network.AWS.CloudFront.DeleteServiceLinkedRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.CloudFront.DeleteServiceLinkedRole
    (
    -- * Creating a Request
      deleteServiceLinkedRole
    , DeleteServiceLinkedRole
    -- * Request Lenses
    , dslrRoleName

    -- * Destructuring the Response
    , deleteServiceLinkedRoleResponse
    , DeleteServiceLinkedRoleResponse
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteServiceLinkedRole' smart constructor.
newtype DeleteServiceLinkedRole = DeleteServiceLinkedRole'
  { _dslrRoleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceLinkedRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dslrRoleName' - Undocumented member.
deleteServiceLinkedRole
    :: Text -- ^ 'dslrRoleName'
    -> DeleteServiceLinkedRole
deleteServiceLinkedRole pRoleName_ =
  DeleteServiceLinkedRole' {_dslrRoleName = pRoleName_}


-- | Undocumented member.
dslrRoleName :: Lens' DeleteServiceLinkedRole Text
dslrRoleName = lens _dslrRoleName (\ s a -> s{_dslrRoleName = a})

instance AWSRequest DeleteServiceLinkedRole where
        type Rs DeleteServiceLinkedRole =
             DeleteServiceLinkedRoleResponse
        request = delete cloudFront
        response
          = receiveNull DeleteServiceLinkedRoleResponse'

instance Hashable DeleteServiceLinkedRole where

instance NFData DeleteServiceLinkedRole where

instance ToHeaders DeleteServiceLinkedRole where
        toHeaders = const mempty

instance ToPath DeleteServiceLinkedRole where
        toPath DeleteServiceLinkedRole'{..}
          = mconcat
              ["/2017-10-30/service-linked-role/",
               toBS _dslrRoleName]

instance ToQuery DeleteServiceLinkedRole where
        toQuery = const mempty

-- | /See:/ 'deleteServiceLinkedRoleResponse' smart constructor.
data DeleteServiceLinkedRoleResponse =
  DeleteServiceLinkedRoleResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteServiceLinkedRoleResponse' with the minimum fields required to make a request.
--
deleteServiceLinkedRoleResponse
    :: DeleteServiceLinkedRoleResponse
deleteServiceLinkedRoleResponse = DeleteServiceLinkedRoleResponse'


instance NFData DeleteServiceLinkedRoleResponse where
