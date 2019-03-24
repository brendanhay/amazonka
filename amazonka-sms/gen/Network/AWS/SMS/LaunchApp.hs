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
-- Module      : Network.AWS.SMS.LaunchApp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Launches an application stack.
--
--
module Network.AWS.SMS.LaunchApp
    (
    -- * Creating a Request
      launchApp
    , LaunchApp
    -- * Request Lenses
    , laAppId

    -- * Destructuring the Response
    , launchAppResponse
    , LaunchAppResponse
    -- * Response Lenses
    , lrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'launchApp' smart constructor.
newtype LaunchApp = LaunchApp'
  { _laAppId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchApp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laAppId' - ID of the application to launch.
launchApp
    :: LaunchApp
launchApp = LaunchApp' {_laAppId = Nothing}


-- | ID of the application to launch.
laAppId :: Lens' LaunchApp (Maybe Text)
laAppId = lens _laAppId (\ s a -> s{_laAppId = a})

instance AWSRequest LaunchApp where
        type Rs LaunchApp = LaunchAppResponse
        request = postJSON sms
        response
          = receiveEmpty
              (\ s h x ->
                 LaunchAppResponse' <$> (pure (fromEnum s)))

instance Hashable LaunchApp where

instance NFData LaunchApp where

instance ToHeaders LaunchApp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.LaunchApp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON LaunchApp where
        toJSON LaunchApp'{..}
          = object (catMaybes [("appId" .=) <$> _laAppId])

instance ToPath LaunchApp where
        toPath = const "/"

instance ToQuery LaunchApp where
        toQuery = const mempty

-- | /See:/ 'launchAppResponse' smart constructor.
newtype LaunchAppResponse = LaunchAppResponse'
  { _lrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LaunchAppResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsResponseStatus' - -- | The response status code.
launchAppResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> LaunchAppResponse
launchAppResponse pResponseStatus_ =
  LaunchAppResponse' {_lrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
lrsResponseStatus :: Lens' LaunchAppResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData LaunchAppResponse where
