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
-- Module      : Network.AWS.DirectoryService.DisableSSO
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables single-sign on for a directory.
--
--
module Network.AWS.DirectoryService.DisableSSO
    (
    -- * Creating a Request
      disableSSO
    , DisableSSO
    -- * Request Lenses
    , dssoUserName
    , dssoPassword
    , dssoDirectoryId

    -- * Destructuring the Response
    , disableSSOResponse
    , DisableSSOResponse
    -- * Response Lenses
    , dssorsResponseStatus
    ) where

import Network.AWS.DirectoryService.Types
import Network.AWS.DirectoryService.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the 'DisableSso' operation.
--
--
--
-- /See:/ 'disableSSO' smart constructor.
data DisableSSO = DisableSSO'
  { _dssoUserName    :: !(Maybe Text)
  , _dssoPassword    :: !(Maybe (Sensitive Text))
  , _dssoDirectoryId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableSSO' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssoUserName' - The username of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. This account must have privileges to remove a service principal name. If the AD Connector service account does not have privileges to remove a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to disable single sign-on and are not stored by the service. The AD Connector service account is not changed.
--
-- * 'dssoPassword' - The password of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
--
-- * 'dssoDirectoryId' - The identifier of the directory for which to disable single-sign on.
disableSSO
    :: Text -- ^ 'dssoDirectoryId'
    -> DisableSSO
disableSSO pDirectoryId_ =
  DisableSSO'
    { _dssoUserName = Nothing
    , _dssoPassword = Nothing
    , _dssoDirectoryId = pDirectoryId_
    }


-- | The username of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. This account must have privileges to remove a service principal name. If the AD Connector service account does not have privileges to remove a service principal name, you can specify an alternate account with the /UserName/ and /Password/ parameters. These credentials are only used to disable single sign-on and are not stored by the service. The AD Connector service account is not changed.
dssoUserName :: Lens' DisableSSO (Maybe Text)
dssoUserName = lens _dssoUserName (\ s a -> s{_dssoUserName = a})

-- | The password of an alternate account to use to disable single-sign on. This is only used for AD Connector directories. For more information, see the /UserName/ parameter.
dssoPassword :: Lens' DisableSSO (Maybe Text)
dssoPassword = lens _dssoPassword (\ s a -> s{_dssoPassword = a}) . mapping _Sensitive

-- | The identifier of the directory for which to disable single-sign on.
dssoDirectoryId :: Lens' DisableSSO Text
dssoDirectoryId = lens _dssoDirectoryId (\ s a -> s{_dssoDirectoryId = a})

instance AWSRequest DisableSSO where
        type Rs DisableSSO = DisableSSOResponse
        request = postJSON directoryService
        response
          = receiveEmpty
              (\ s h x ->
                 DisableSSOResponse' <$> (pure (fromEnum s)))

instance Hashable DisableSSO where

instance NFData DisableSSO where

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
              (catMaybes
                 [("UserName" .=) <$> _dssoUserName,
                  ("Password" .=) <$> _dssoPassword,
                  Just ("DirectoryId" .= _dssoDirectoryId)])

instance ToPath DisableSSO where
        toPath = const "/"

instance ToQuery DisableSSO where
        toQuery = const mempty

-- | Contains the results of the 'DisableSso' operation.
--
--
--
-- /See:/ 'disableSSOResponse' smart constructor.
newtype DisableSSOResponse = DisableSSOResponse'
  { _dssorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisableSSOResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssorsResponseStatus' - -- | The response status code.
disableSSOResponse
    :: Int -- ^ 'dssorsResponseStatus'
    -> DisableSSOResponse
disableSSOResponse pResponseStatus_ =
  DisableSSOResponse' {_dssorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dssorsResponseStatus :: Lens' DisableSSOResponse Int
dssorsResponseStatus = lens _dssorsResponseStatus (\ s a -> s{_dssorsResponseStatus = a})

instance NFData DisableSSOResponse where
