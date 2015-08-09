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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for a directory and assigns the alias to the directory.
-- The alias is used to construct the access URL for the directory, such as
-- @http:\/\/\<alias>.awsapps.com@.
--
-- After an alias has been created, it cannot be deleted or reused, so this
-- operation should only be used when absolutely necessary.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_CreateAlias.html AWS API Reference> for CreateAlias.
module Network.AWS.DirectoryService.CreateAlias
    (
    -- * Creating a Request
      CreateAlias
    , createAlias
    -- * Request Lenses
    , caDirectoryId
    , caAlias

    -- * Destructuring the Response
    , CreateAliasResponse
    , createAliasResponse
    -- * Response Lenses
    , carsDirectoryId
    , carsAlias
    , carsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.Types.Product
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAlias' smart constructor.
createAlias :: Text -> Text -> CreateAlias
createAlias pDirectoryId_ pAlias_ =
    CreateAlias'
    { _caDirectoryId = pDirectoryId_
    , _caAlias = pAlias_
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
                     (pure (fromEnum s)))

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
-- * 'carsDirectoryId'
--
-- * 'carsAlias'
--
-- * 'carsStatus'
data CreateAliasResponse = CreateAliasResponse'
    { _carsDirectoryId :: !(Maybe Text)
    , _carsAlias       :: !(Maybe Text)
    , _carsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAliasResponse' smart constructor.
createAliasResponse :: Int -> CreateAliasResponse
createAliasResponse pStatus_ =
    CreateAliasResponse'
    { _carsDirectoryId = Nothing
    , _carsAlias = Nothing
    , _carsStatus = pStatus_
    }

-- | The identifier of the directory.
carsDirectoryId :: Lens' CreateAliasResponse (Maybe Text)
carsDirectoryId = lens _carsDirectoryId (\ s a -> s{_carsDirectoryId = a});

-- | The alias for the directory.
carsAlias :: Lens' CreateAliasResponse (Maybe Text)
carsAlias = lens _carsAlias (\ s a -> s{_carsAlias = a});

-- | Undocumented member.
carsStatus :: Lens' CreateAliasResponse Int
carsStatus = lens _carsStatus (\ s a -> s{_carsStatus = a});
