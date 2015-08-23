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
-- Module      : Network.AWS.DirectoryService.EnableSSO
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables single-sign on for a directory.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_EnableSSO.html AWS API Reference> for EnableSSO.
module Network.AWS.DirectoryService.EnableSSO
    (
    -- * Creating a Request
      enableSSO
    , EnableSSO
    -- * Request Lenses
    , esUserName
    , esPassword
    , esDirectoryId

    -- * Destructuring the Response
    , enableSSOResponse
    , EnableSSOResponse
    -- * Response Lenses
    , esrsStatus
    ) where

import           Network.AWS.DirectoryService.Types
import           Network.AWS.DirectoryService.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the EnableSso operation.
--
-- /See:/ 'enableSSO' smart constructor.
data EnableSSO = EnableSSO'
    { _esUserName    :: !(Maybe Text)
    , _esPassword    :: !(Maybe (Sensitive Text))
    , _esDirectoryId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableSSO' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esUserName'
--
-- * 'esPassword'
--
-- * 'esDirectoryId'
enableSSO
    :: Text -- ^ 'esDirectoryId'
    -> EnableSSO
enableSSO pDirectoryId_ =
    EnableSSO'
    { _esUserName = Nothing
    , _esPassword = Nothing
    , _esDirectoryId = pDirectoryId_
    }

-- | The username of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. This account must have
-- privileges to add a service principle name.
--
-- If the AD Connector service account does not have privileges to add a
-- service principle name, you can specify an alternate account with the
-- /UserName/ and /Password/ parameters. These credentials are only used to
-- enable single sign-on and are not stored by the service. The AD
-- Connector service account is not changed.
esUserName :: Lens' EnableSSO (Maybe Text)
esUserName = lens _esUserName (\ s a -> s{_esUserName = a});

-- | The password of an alternate account to use to enable single-sign on.
-- This is only used for AD Connector directories. See the /UserName/
-- parameter for more information.
esPassword :: Lens' EnableSSO (Maybe Text)
esPassword = lens _esPassword (\ s a -> s{_esPassword = a}) . mapping _Sensitive;

-- | The identifier of the directory to enable single-sign on for.
esDirectoryId :: Lens' EnableSSO Text
esDirectoryId = lens _esDirectoryId (\ s a -> s{_esDirectoryId = a});

instance AWSRequest EnableSSO where
        type Sv EnableSSO = DirectoryService
        type Rs EnableSSO = EnableSSOResponse
        request = postJSON
        response
          = receiveEmpty
              (\ s h x ->
                 EnableSSOResponse' <$> (pure (fromEnum s)))

instance ToHeaders EnableSSO where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DirectoryService_20150416.EnableSso" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON EnableSSO where
        toJSON EnableSSO'{..}
          = object
              (catMaybes
                 [("UserName" .=) <$> _esUserName,
                  ("Password" .=) <$> _esPassword,
                  Just ("DirectoryId" .= _esDirectoryId)])

instance ToPath EnableSSO where
        toPath = const "/"

instance ToQuery EnableSSO where
        toQuery = const mempty

-- | Contains the results of the EnableSso operation.
--
-- /See:/ 'enableSSOResponse' smart constructor.
newtype EnableSSOResponse = EnableSSOResponse'
    { _esrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EnableSSOResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsStatus'
enableSSOResponse
    :: Int -- ^ 'esrsStatus'
    -> EnableSSOResponse
enableSSOResponse pStatus_ =
    EnableSSOResponse'
    { _esrsStatus = pStatus_
    }

-- | The response status code.
esrsStatus :: Lens' EnableSSOResponse Int
esrsStatus = lens _esrsStatus (\ s a -> s{_esrsStatus = a});
