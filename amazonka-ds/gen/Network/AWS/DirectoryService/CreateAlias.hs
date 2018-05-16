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
-- Module      : Network.AWS.DirectoryService.CreateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a directory and assigns the alias to the directory. The alias is used to construct the access URL for the directory, such as @http://<alias>.awsapps.com@ .
--
--
-- /Important:/ After an alias has been created, it cannot be deleted or reused, so this operation should only be used when absolutely necessary.
--
module Network.AWS.DirectoryService.CreateAlias
    (
    -- * Creating a Request
      createAlias
    , CreateAlias
    -- * Request Lenses
    , caDirectoryId
    , caAlias

    -- * Destructuring the Response
    , createAliasResponse
    , CreateAliasResponse
    -- * Response Lenses
    , carsDirectoryId
    , carsAlias
    , carsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'CreateAlias' operation.
--
--
--
-- /See:/ 'createAlias' smart constructor.
data CreateAlias = CreateAlias'
  { _caDirectoryId :: !Text
  , _caAlias       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caDirectoryId' - The identifier of the directory for which to create the alias.
--
-- * 'caAlias' - The requested alias. The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
createAlias
    :: Text -- ^ 'caDirectoryId'
    -> Text -- ^ 'caAlias'
    -> CreateAlias
createAlias pDirectoryId_ pAlias_ =
  CreateAlias' {_caDirectoryId = pDirectoryId_, _caAlias = pAlias_}


-- | The identifier of the directory for which to create the alias.
caDirectoryId :: Lens' CreateAlias Text
caDirectoryId = lens _caDirectoryId (\ s a -> s{_caDirectoryId = a})

-- | The requested alias. The alias must be unique amongst all aliases in AWS. This operation throws an @EntityAlreadyExistsException@ error if the alias already exists.
caAlias :: Lens' CreateAlias Text
caAlias = lens _caAlias (\ s a -> s{_caAlias = a})

instance AWSRequest CreateAlias where
        type Rs CreateAlias = CreateAliasResponse
        request = postJSON directoryService
        response
          = receiveJSON
              (\ s h x ->
                 CreateAliasResponse' <$>
                   (x .?> "DirectoryId") <*> (x .?> "Alias") <*>
                     (pure (fromEnum s)))

instance Hashable CreateAlias where

instance NFData CreateAlias where

instance ToHeaders CreateAlias where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.CreateAlias" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateAlias where
        toJSON CreateAlias'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _caDirectoryId),
                  Just ("Alias" .= _caAlias)])

instance ToPath CreateAlias where
        toPath = const "/"

instance ToQuery CreateAlias where
        toQuery = const mempty

-- | Contains the results of the 'CreateAlias' operation.
--
--
--
-- /See:/ 'createAliasResponse' smart constructor.
data CreateAliasResponse = CreateAliasResponse'
  { _carsDirectoryId    :: !(Maybe Text)
  , _carsAlias          :: !(Maybe Text)
  , _carsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAliasResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'carsDirectoryId' - The identifier of the directory.
--
-- * 'carsAlias' - The alias for the directory.
--
-- * 'carsResponseStatus' - -- | The response status code.
createAliasResponse
    :: Int -- ^ 'carsResponseStatus'
    -> CreateAliasResponse
createAliasResponse pResponseStatus_ =
  CreateAliasResponse'
    { _carsDirectoryId = Nothing
    , _carsAlias = Nothing
    , _carsResponseStatus = pResponseStatus_
    }


-- | The identifier of the directory.
carsDirectoryId :: Lens' CreateAliasResponse (Maybe Text)
carsDirectoryId = lens _carsDirectoryId (\ s a -> s{_carsDirectoryId = a})

-- | The alias for the directory.
carsAlias :: Lens' CreateAliasResponse (Maybe Text)
carsAlias = lens _carsAlias (\ s a -> s{_carsAlias = a})

-- | -- | The response status code.
carsResponseStatus :: Lens' CreateAliasResponse Int
carsResponseStatus = lens _carsResponseStatus (\ s a -> s{_carsResponseStatus = a})

instance NFData CreateAliasResponse where
