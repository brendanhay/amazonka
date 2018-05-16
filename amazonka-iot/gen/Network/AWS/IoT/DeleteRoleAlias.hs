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
-- Module      : Network.AWS.IoT.DeleteRoleAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a role alias
--
--
module Network.AWS.IoT.DeleteRoleAlias
    (
    -- * Creating a Request
      deleteRoleAlias
    , DeleteRoleAlias
    -- * Request Lenses
    , dRoleAlias

    -- * Destructuring the Response
    , deleteRoleAliasResponse
    , DeleteRoleAliasResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRoleAlias' smart constructor.
newtype DeleteRoleAlias = DeleteRoleAlias'
  { _dRoleAlias :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoleAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dRoleAlias' - The role alias to delete.
deleteRoleAlias
    :: Text -- ^ 'dRoleAlias'
    -> DeleteRoleAlias
deleteRoleAlias pRoleAlias_ = DeleteRoleAlias' {_dRoleAlias = pRoleAlias_}


-- | The role alias to delete.
dRoleAlias :: Lens' DeleteRoleAlias Text
dRoleAlias = lens _dRoleAlias (\ s a -> s{_dRoleAlias = a})

instance AWSRequest DeleteRoleAlias where
        type Rs DeleteRoleAlias = DeleteRoleAliasResponse
        request = delete ioT
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteRoleAliasResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteRoleAlias where

instance NFData DeleteRoleAlias where

instance ToHeaders DeleteRoleAlias where
        toHeaders = const mempty

instance ToPath DeleteRoleAlias where
        toPath DeleteRoleAlias'{..}
          = mconcat ["/role-aliases/", toBS _dRoleAlias]

instance ToQuery DeleteRoleAlias where
        toQuery = const mempty

-- | /See:/ 'deleteRoleAliasResponse' smart constructor.
newtype DeleteRoleAliasResponse = DeleteRoleAliasResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRoleAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteRoleAliasResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteRoleAliasResponse
deleteRoleAliasResponse pResponseStatus_ =
  DeleteRoleAliasResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteRoleAliasResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteRoleAliasResponse where
