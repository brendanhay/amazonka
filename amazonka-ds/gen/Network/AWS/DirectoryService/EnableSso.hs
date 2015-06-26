{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectoryService.EnableSso
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

-- | Enables single-sign on for a directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_EnableSso.html>
module Network.AWS.DirectoryService.EnableSso
    (
    -- * Request
      EnableSso
    -- ** Request constructor
    , enableSso
    -- ** Request lenses
    , esUserName
    , esPassword
    , esDirectoryId

    -- * Response
    , EnableSsoResponse
    -- ** Response constructor
    , enableSsoResponse
    -- ** Response lenses
    , esrStatusCode
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the EnableSso operation.
--
-- /See:/ 'enableSso' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esUserName'
--
-- * 'esPassword'
--
-- * 'esDirectoryId'
data EnableSso = EnableSso'{_esUserName :: Maybe Text, _esPassword :: Maybe (Sensitive Text), _esDirectoryId :: Text} deriving (Eq, Read, Show)

-- | 'EnableSso' smart constructor.
enableSso :: Text -> EnableSso
enableSso pDirectoryId = EnableSso'{_esUserName = Nothing, _esPassword = Nothing, _esDirectoryId = pDirectoryId};

-- | The username of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to add a service principle name.
--
-- If the AD Connector service account does not have privileges to add a
-- service principle name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- enable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
esUserName :: Lens' EnableSso (Maybe Text)
esUserName = lens _esUserName (\ s a -> s{_esUserName = a});

-- | The password of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. See the /UserName/
-- parameter for more information.
esPassword :: Lens' EnableSso (Maybe Text)
esPassword = lens _esPassword (\ s a -> s{_esPassword = a}) . mapping _Sensitive;

-- | The identifier of the directory to enable single-sign on for.
esDirectoryId :: Lens' EnableSso Text
esDirectoryId = lens _esDirectoryId (\ s a -> s{_esDirectoryId = a});

instance AWSRequest EnableSso where
        type Sv EnableSso = DirectoryService
        type Rs EnableSso = EnableSsoResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 EnableSsoResponse' <$> (pure (fromEnum s)))

instance ToHeaders EnableSso where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.EnableSso" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableSso where
        toJSON EnableSso'{..}
          = object
              ["UserName" .= _esUserName,
               "Password" .= _esPassword,
               "DirectoryId" .= _esDirectoryId]

instance ToPath EnableSso where
        toPath = const "/"

instance ToQuery EnableSso where
        toQuery = const mempty

-- | Contains the results of the EnableSso operation.
--
-- /See:/ 'enableSsoResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'esrStatusCode'
newtype EnableSsoResponse = EnableSsoResponse'{_esrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'EnableSsoResponse' smart constructor.
enableSsoResponse :: Int -> EnableSsoResponse
enableSsoResponse pStatusCode = EnableSsoResponse'{_esrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
esrStatusCode :: Lens' EnableSsoResponse Int
esrStatusCode = lens _esrStatusCode (\ s a -> s{_esrStatusCode = a});
