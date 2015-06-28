{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.DirectoryService.CreateAlias
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an alias for a directory and assigns the alias to the directory.
-- The alias is used to construct the access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@.
--
-- After an alias has been created, it cannot be deleted or reused, so this
-- operation should only be used when absolutely necessary.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateAlias.html>
module Network.AWS.DirectoryService.CreateAlias
    (
    -- * Request
      CreateAlias
    -- ** Request constructor
    , createAlias
    -- ** Request lenses
    , caDirectoryId
    , caAlias

    -- * Response
    , CreateAliasResponse
    -- ** Response constructor
    , createAliasResponse
    -- ** Response lenses
    , carDirectoryId
    , carAlias
    , carStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateAlias operation.
--
-- /See:/ 'createAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caDirectoryId'
--
-- * 'caAlias'
data CreateAlias = CreateAlias'
    { _caDirectoryId :: !Text
    , _caAlias       :: !Text
    } deriving (Eq,Read,Show)

-- | 'CreateAlias' smart constructor.
createAlias :: Text -> Text -> CreateAlias
createAlias pDirectoryId pAlias =
    CreateAlias'
    { _caDirectoryId = pDirectoryId
    , _caAlias = pAlias
    }

-- | The identifier of the directory to create the alias for.
caDirectoryId :: Lens' CreateAlias Text
caDirectoryId = lens _caDirectoryId (\ s a -> s{_caDirectoryId = a});

-- | The requested alias.
--
-- The alias must be unique amongst all aliases in AWS. This operation will
-- throw an @EntityAlreadyExistsException@ if this alias already exists.
caAlias :: Lens' CreateAlias Text
caAlias = lens _caAlias (\ s a -> s{_caAlias = a});

instance AWSRequest CreateAlias where
        type Sv CreateAlias = DirectoryService
        type Rs CreateAlias = CreateAliasResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateAliasResponse' <$>
                   (x .?> "DirectoryId") <*> (x .?> "Alias") <*>
                     (pure s))

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
              ["DirectoryId" .= _caDirectoryId,
               "Alias" .= _caAlias]

instance ToPath CreateAlias where
        toPath = const "/"

instance ToQuery CreateAlias where
        toQuery = const mempty

-- | Contains the results of the CreateAlias operation.
--
-- /See:/ 'createAliasResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'carDirectoryId'
--
-- * 'carAlias'
--
-- * 'carStatus'
data CreateAliasResponse = CreateAliasResponse'
    { _carDirectoryId :: !(Maybe Text)
    , _carAlias       :: !(Maybe Text)
    , _carStatus      :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateAliasResponse' smart constructor.
createAliasResponse :: Status -> CreateAliasResponse
createAliasResponse pStatus =
    CreateAliasResponse'
    { _carDirectoryId = Nothing
    , _carAlias = Nothing
    , _carStatus = pStatus
    }

-- | The identifier of the directory.
carDirectoryId :: Lens' CreateAliasResponse (Maybe Text)
carDirectoryId = lens _carDirectoryId (\ s a -> s{_carDirectoryId = a});

-- | The alias for the directory.
carAlias :: Lens' CreateAliasResponse (Maybe Text)
carAlias = lens _carAlias (\ s a -> s{_carAlias = a});

-- | FIXME: Undocumented member.
carStatus :: Lens' CreateAliasResponse Status
carStatus = lens _carStatus (\ s a -> s{_carStatus = a});
