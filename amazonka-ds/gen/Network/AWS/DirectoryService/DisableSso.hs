{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DisableSso
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DisableSso.html>
module Network.AWS.DirectoryService.DisableSso
    (
    -- * Request
      DisableSso
    -- ** Request constructor
    , disableSso
    -- ** Request lenses
    , drqUserName
    , drqPassword
    , drqDirectoryId

    -- * Response
    , DisableSsoResponse
    -- ** Response constructor
    , disableSsoResponse
    -- ** Response lenses
    , drsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the DisableSso operation.
--
-- /See:/ 'disableSso' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqUserName'
--
-- * 'drqPassword'
--
-- * 'drqDirectoryId'
data DisableSso = DisableSso'
    { _drqUserName    :: !(Maybe Text)
    , _drqPassword    :: !(Maybe (Sensitive Text))
    , _drqDirectoryId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableSso' smart constructor.
disableSso :: Text -> DisableSso
disableSso pDirectoryId =
    DisableSso'
    { _drqUserName = Nothing
    , _drqPassword = Nothing
    , _drqDirectoryId = pDirectoryId
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
drqUserName :: Lens' DisableSso (Maybe Text)
drqUserName = lens _drqUserName (\ s a -> s{_drqUserName = a});

-- | The password of an alternate account to use to disable single-sign on.
-- This is only used for AD Connector directories. See the /UserName/
-- parameter for more information.
drqPassword :: Lens' DisableSso (Maybe Text)
drqPassword = lens _drqPassword (\ s a -> s{_drqPassword = a}) . mapping _Sensitive;

-- | The identifier of the directory to disable single-sign on for.
drqDirectoryId :: Lens' DisableSso Text
drqDirectoryId = lens _drqDirectoryId (\ s a -> s{_drqDirectoryId = a});

instance AWSRequest DisableSso where
        type Sv DisableSso = DirectoryService
        type Rs DisableSso = DisableSsoResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DisableSsoResponse' <$> (pure (fromEnum s)))

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
              ["UserName" .= _drqUserName,
               "Password" .= _drqPassword,
               "DirectoryId" .= _drqDirectoryId]

instance ToPath DisableSso where
        toPath = const "/"

instance ToQuery DisableSso where
        toQuery = const mempty

-- | Contains the results of the DisableSso operation.
--
-- /See:/ 'disableSsoResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsStatus'
newtype DisableSsoResponse = DisableSsoResponse'
    { _drsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DisableSsoResponse' smart constructor.
disableSsoResponse :: Int -> DisableSsoResponse
disableSsoResponse pStatus =
    DisableSsoResponse'
    { _drsStatus = pStatus
    }

-- | FIXME: Undocumented member.
drsStatus :: Lens' DisableSsoResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
