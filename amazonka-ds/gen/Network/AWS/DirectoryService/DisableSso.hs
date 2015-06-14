{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.DirectoryService.DisableSso
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

-- | Disables single-sign on for a directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DisableSso.html>
module Network.AWS.DirectoryService.DisableSso
    (
    -- * Request
      DisableSso
    -- ** Request constructor
    , disableSso
    -- ** Request lenses
    , disDirectoryId
    , disUserName
    , disPassword

    -- * Response
    , DisableSsoResponse
    -- ** Response constructor
    , disableSsoResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.DirectoryService.Types

-- | /See:/ 'disableSso' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disDirectoryId'
--
-- * 'disUserName'
--
-- * 'disPassword'
data DisableSso = DisableSso'{_disDirectoryId :: Text, _disUserName :: Text, _disPassword :: Sensitive Text} deriving (Eq, Read, Show)

-- | 'DisableSso' smart constructor.
disableSso :: Text -> Text -> Text -> DisableSso
disableSso pDirectoryId pUserName pPassword = DisableSso'{_disDirectoryId = pDirectoryId, _disUserName = pUserName, _disPassword = _Sensitive # pPassword};

-- | The identifier of the directory to disable single-sign on for.
disDirectoryId :: Lens' DisableSso Text
disDirectoryId = lens _disDirectoryId (\ s a -> s{_disDirectoryId = a});

-- | The username of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to remove a service principle name.
--
-- If the AD Connector service account does not have privileges to remove a
-- service principle name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- disable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
disUserName :: Lens' DisableSso Text
disUserName = lens _disUserName (\ s a -> s{_disUserName = a});

-- | The password of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. See the /UserName/
-- parameter for more information.
disPassword :: Lens' DisableSso Text
disPassword = lens _disPassword (\ s a -> s{_disPassword = a}) . _Sensitive;

instance AWSRequest DisableSso where
        type Sv DisableSso = DirectoryService
        type Rs DisableSso = DisableSsoResponse
        request = postJSON
        response = receiveNull DisableSsoResponse'

instance ToHeaders DisableSso where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DisableSso" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableSso where
        toJSON DisableSso'{..}
          = object
              ["DirectoryId" .= _disDirectoryId,
               "UserName" .= _disUserName,
               "Password" .= _disPassword]

instance ToPath DisableSso where
        toPath = const "/"

instance ToQuery DisableSso where
        toQuery = const mempty

-- | /See:/ 'disableSsoResponse' smart constructor.
data DisableSsoResponse = DisableSsoResponse' deriving (Eq, Read, Show)

-- | 'DisableSsoResponse' smart constructor.
disableSsoResponse :: DisableSsoResponse
disableSsoResponse = DisableSsoResponse';
