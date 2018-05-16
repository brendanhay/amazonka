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
-- Module      : Network.AWS.DeviceFarm.InstallToRemoteAccessSession
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Installs an application to the device in a remote access session. For Android applications, the file must be in .apk format. For iOS applications, the file must be in .ipa format.
--
--
module Network.AWS.DeviceFarm.InstallToRemoteAccessSession
    (
    -- * Creating a Request
      installToRemoteAccessSession
    , InstallToRemoteAccessSession
    -- * Request Lenses
    , itrasRemoteAccessSessionARN
    , itrasAppARN

    -- * Destructuring the Response
    , installToRemoteAccessSessionResponse
    , InstallToRemoteAccessSessionResponse
    -- * Response Lenses
    , itrasrsAppUpload
    , itrasrsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to install an Android application (in .apk format) or an iOS application (in .ipa format) as part of a remote access session.
--
--
--
-- /See:/ 'installToRemoteAccessSession' smart constructor.
data InstallToRemoteAccessSession = InstallToRemoteAccessSession'
  { _itrasRemoteAccessSessionARN :: !Text
  , _itrasAppARN                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstallToRemoteAccessSession' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itrasRemoteAccessSessionARN' - The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
--
-- * 'itrasAppARN' - The Amazon Resource Name (ARN) of the app about which you are requesting information.
installToRemoteAccessSession
    :: Text -- ^ 'itrasRemoteAccessSessionARN'
    -> Text -- ^ 'itrasAppARN'
    -> InstallToRemoteAccessSession
installToRemoteAccessSession pRemoteAccessSessionARN_ pAppARN_ =
  InstallToRemoteAccessSession'
    { _itrasRemoteAccessSessionARN = pRemoteAccessSessionARN_
    , _itrasAppARN = pAppARN_
    }


-- | The Amazon Resource Name (ARN) of the remote access session about which you are requesting information.
itrasRemoteAccessSessionARN :: Lens' InstallToRemoteAccessSession Text
itrasRemoteAccessSessionARN = lens _itrasRemoteAccessSessionARN (\ s a -> s{_itrasRemoteAccessSessionARN = a})

-- | The Amazon Resource Name (ARN) of the app about which you are requesting information.
itrasAppARN :: Lens' InstallToRemoteAccessSession Text
itrasAppARN = lens _itrasAppARN (\ s a -> s{_itrasAppARN = a})

instance AWSRequest InstallToRemoteAccessSession
         where
        type Rs InstallToRemoteAccessSession =
             InstallToRemoteAccessSessionResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 InstallToRemoteAccessSessionResponse' <$>
                   (x .?> "appUpload") <*> (pure (fromEnum s)))

instance Hashable InstallToRemoteAccessSession where

instance NFData InstallToRemoteAccessSession where

instance ToHeaders InstallToRemoteAccessSession where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.InstallToRemoteAccessSession"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON InstallToRemoteAccessSession where
        toJSON InstallToRemoteAccessSession'{..}
          = object
              (catMaybes
                 [Just
                    ("remoteAccessSessionArn" .=
                       _itrasRemoteAccessSessionARN),
                  Just ("appArn" .= _itrasAppARN)])

instance ToPath InstallToRemoteAccessSession where
        toPath = const "/"

instance ToQuery InstallToRemoteAccessSession where
        toQuery = const mempty

-- | Represents the response from the server after AWS Device Farm makes a request to install to a remote access session.
--
--
--
-- /See:/ 'installToRemoteAccessSessionResponse' smart constructor.
data InstallToRemoteAccessSessionResponse = InstallToRemoteAccessSessionResponse'
  { _itrasrsAppUpload      :: !(Maybe Upload)
  , _itrasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InstallToRemoteAccessSessionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'itrasrsAppUpload' - An app to upload or that has been uploaded.
--
-- * 'itrasrsResponseStatus' - -- | The response status code.
installToRemoteAccessSessionResponse
    :: Int -- ^ 'itrasrsResponseStatus'
    -> InstallToRemoteAccessSessionResponse
installToRemoteAccessSessionResponse pResponseStatus_ =
  InstallToRemoteAccessSessionResponse'
    {_itrasrsAppUpload = Nothing, _itrasrsResponseStatus = pResponseStatus_}


-- | An app to upload or that has been uploaded.
itrasrsAppUpload :: Lens' InstallToRemoteAccessSessionResponse (Maybe Upload)
itrasrsAppUpload = lens _itrasrsAppUpload (\ s a -> s{_itrasrsAppUpload = a})

-- | -- | The response status code.
itrasrsResponseStatus :: Lens' InstallToRemoteAccessSessionResponse Int
itrasrsResponseStatus = lens _itrasrsResponseStatus (\ s a -> s{_itrasrsResponseStatus = a})

instance NFData InstallToRemoteAccessSessionResponse
         where
