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
-- Module      : Network.AWS.WorkMail.DeleteAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove the alias from a set of aliases for a given user.
--
--
module Network.AWS.WorkMail.DeleteAlias
    (
    -- * Creating a Request
      deleteAlias
    , DeleteAlias
    -- * Request Lenses
    , daOrganizationId
    , daEntityId
    , daAlias

    -- * Destructuring the Response
    , deleteAliasResponse
    , DeleteAliasResponse
    -- * Response Lenses
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'deleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { _daOrganizationId :: !Text
  , _daEntityId       :: !Text
  , _daAlias          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daOrganizationId' - The identifier for the organization under which the user exists.
--
-- * 'daEntityId' - The identifier for the Amazon WorkMail entity to have the aliases removed.
--
-- * 'daAlias' - The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
deleteAlias
    :: Text -- ^ 'daOrganizationId'
    -> Text -- ^ 'daEntityId'
    -> Text -- ^ 'daAlias'
    -> DeleteAlias
deleteAlias pOrganizationId_ pEntityId_ pAlias_ =
  DeleteAlias'
    { _daOrganizationId = pOrganizationId_
    , _daEntityId = pEntityId_
    , _daAlias = pAlias_
    }


-- | The identifier for the organization under which the user exists.
daOrganizationId :: Lens' DeleteAlias Text
daOrganizationId = lens _daOrganizationId (\ s a -> s{_daOrganizationId = a})

-- | The identifier for the Amazon WorkMail entity to have the aliases removed.
daEntityId :: Lens' DeleteAlias Text
daEntityId = lens _daEntityId (\ s a -> s{_daEntityId = a})

-- | The aliases to be removed from the user's set of aliases. Duplicate entries in the list are collapsed into single entries (the list is transformed into a set).
daAlias :: Lens' DeleteAlias Text
daAlias = lens _daAlias (\ s a -> s{_daAlias = a})

instance AWSRequest DeleteAlias where
        type Rs DeleteAlias = DeleteAliasResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteAliasResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteAlias where

instance NFData DeleteAlias where

instance ToHeaders DeleteAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DeleteAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteAlias where
        toJSON DeleteAlias'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _daOrganizationId),
                  Just ("EntityId" .= _daEntityId),
                  Just ("Alias" .= _daAlias)])

instance ToPath DeleteAlias where
        toPath = const "/"

instance ToQuery DeleteAlias where
        toQuery = const mempty

-- | /See:/ 'deleteAliasResponse' smart constructor.
newtype DeleteAliasResponse = DeleteAliasResponse'
  { _darsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteAliasResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DeleteAliasResponse
deleteAliasResponse pResponseStatus_ =
  DeleteAliasResponse' {_darsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAliasResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DeleteAliasResponse where
