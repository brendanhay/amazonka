{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableSSO
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DisableSSO.html AWS API Reference> for DisableSSO.
module Network.AWS.DirectoryService.DisableSSO
    (
    -- * Creating a Request
      DisableSSO
    , disableSSO
    -- * Request Lenses
    , dssoUserName
    , dssoPassword
    , dssoDirectoryId

    -- * Destructuring the Response
    , DisableSSOResponse
    , disableSSOResponse
    -- * Response Lenses
    , dssorsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DisableSso operation.
--
-- /See:/ 'disableSSO' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssoUserName'
--
-- * 'dssoPassword'
--
-- * 'dssoDirectoryId'
data DisableSSO = DisableSSO'
    { _dssoUserName    :: !(Maybe Text)
    , _dssoPassword    :: !(Maybe (Sensitive Text))
    , _dssoDirectoryId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableSSO' smart constructor.
disableSSO :: Text -> DisableSSO
disableSSO pDirectoryId_ =
    DisableSSO'
    { _dssoUserName = Nothing
    , _dssoPassword = Nothing
    , _dssoDirectoryId = pDirectoryId_
    }

-- | The username of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to remove a service principle name.
--
-- If the AD Connector service account does not have privileges to remove a
-- service principle name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- disable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
dssoUserName :: Lens' DisableSSO (Maybe Text)
dssoUserName = lens _dssoUserName (\ s a -> s{_dssoUserName = a});

-- | The password of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. See the /UserName/
-- parameter for more information.
dssoPassword :: Lens' DisableSSO (Maybe Text)
dssoPassword = lens _dssoPassword (\ s a -> s{_dssoPassword = a}) . mapping _Sensitive;

-- | The identifier of the directory to disable single-sign on for.
dssoDirectoryId :: Lens' DisableSSO Text
dssoDirectoryId = lens _dssoDirectoryId (\ s a -> s{_dssoDirectoryId = a});

instance AWSRequest DisableSSO where
        type Sv DisableSSO = DirectoryService
        type Rs DisableSSO = DisableSSOResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DisableSSOResponse' <$> (pure (fromEnum s)))

instance ToHeaders DisableSSO where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.DisableSso" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisableSSO where
        toJSON DisableSSO'{..}
          = object
              ["UserName" .= _dssoUserName,
               "Password" .= _dssoPassword,
               "DirectoryId" .= _dssoDirectoryId]

instance ToPath DisableSSO where
        toPath = const "/"

instance ToQuery DisableSSO where
        toQuery = const mempty

-- | Contains the results of the DisableSso operation.
--
-- /See:/ 'disableSSOResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssorsStatus'
newtype DisableSSOResponse = DisableSSOResponse'
    { _dssorsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableSSOResponse' smart constructor.
disableSSOResponse :: Int -> DisableSSOResponse
disableSSOResponse pStatus_ =
    DisableSSOResponse'
    { _dssorsStatus = pStatus_
    }

-- | Undocumented member.
dssorsStatus :: Lens' DisableSSOResponse Int
dssorsStatus = lens _dssorsStatus (\ s a -> s{_dssorsStatus = a});
