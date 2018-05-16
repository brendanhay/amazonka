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
-- Module      : Network.AWS.WorkMail.CreateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an alias to the set of a given member of Amazon WorkMail.
--
--
module Network.AWS.WorkMail.CreateAlias
    (
    -- * Creating a Request
      createAlias
    , CreateAlias
    -- * Request Lenses
    , caOrganizationId
    , caEntityId
    , caAlias

    -- * Destructuring the Response
    , createAliasResponse
    , CreateAliasResponse
    -- * Response Lenses
    , carsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'createAlias' smart constructor.
data CreateAlias = CreateAlias'
  { _caOrganizationId :: !Text
  , _caEntityId       :: !Text
  , _caAlias          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caOrganizationId' - The organization under which the member exists.
--
-- * 'caEntityId' - The alias is added to this Amazon WorkMail entity.
--
-- * 'caAlias' - The alias to add to the user.
createAlias
    :: Text -- ^ 'caOrganizationId'
    -> Text -- ^ 'caEntityId'
    -> Text -- ^ 'caAlias'
    -> CreateAlias
createAlias pOrganizationId_ pEntityId_ pAlias_ =
  CreateAlias'
    { _caOrganizationId = pOrganizationId_
    , _caEntityId = pEntityId_
    , _caAlias = pAlias_
    }


-- | The organization under which the member exists.
caOrganizationId :: Lens' CreateAlias Text
caOrganizationId = lens _caOrganizationId (\ s a -> s{_caOrganizationId = a})

-- | The alias is added to this Amazon WorkMail entity.
caEntityId :: Lens' CreateAlias Text
caEntityId = lens _caEntityId (\ s a -> s{_caEntityId = a})

-- | The alias to add to the user.
caAlias :: Lens' CreateAlias Text
caAlias = lens _caAlias (\ s a -> s{_caAlias = a})

instance AWSRequest CreateAlias where
        type Rs CreateAlias = CreateAliasResponse
        request = postJSON workMail
        response
          = receiveEmpty
              (\ s h x ->
                 CreateAliasResponse' <$> (pure (fromEnum s)))

instance Hashable CreateAlias where

instance NFData CreateAlias where

instance ToHeaders CreateAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.CreateAlias" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAlias where
        toJSON CreateAlias'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _caOrganizationId),
                  Just ("EntityId" .= _caEntityId),
                  Just ("Alias" .= _caAlias)])

instance ToPath CreateAlias where
        toPath = const "/"

instance ToQuery CreateAlias where
        toQuery = const mempty

-- | /See:/ 'createAliasResponse' smart constructor.
newtype CreateAliasResponse = CreateAliasResponse'
  { _carsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsResponseStatus' - -- | The response status code.
createAliasResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAliasResponse
createAliasResponse pResponseStatus_ =
  CreateAliasResponse' {_carsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAliasResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAliasResponse where
