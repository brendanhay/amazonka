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
-- Module      : Network.AWS.AppStream.CreateImageBuilderStreamingURL
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a URL to start an image builder streaming session.
--
--
module Network.AWS.AppStream.CreateImageBuilderStreamingURL
    (
    -- * Creating a Request
      createImageBuilderStreamingURL
    , CreateImageBuilderStreamingURL
    -- * Request Lenses
    , cibsuValidity
    , cibsuName

    -- * Destructuring the Response
    , createImageBuilderStreamingURLResponse
    , CreateImageBuilderStreamingURLResponse
    -- * Response Lenses
    , cibsursStreamingURL
    , cibsursExpires
    , cibsursResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createImageBuilderStreamingURL' smart constructor.
data CreateImageBuilderStreamingURL = CreateImageBuilderStreamingURL'
  { _cibsuValidity :: !(Maybe Integer)
  , _cibsuName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateImageBuilderStreamingURL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibsuValidity' - The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
--
-- * 'cibsuName' - The name of the image builder.
createImageBuilderStreamingURL
    :: Text -- ^ 'cibsuName'
    -> CreateImageBuilderStreamingURL
createImageBuilderStreamingURL pName_ =
  CreateImageBuilderStreamingURL'
    {_cibsuValidity = Nothing, _cibsuName = pName_}


-- | The time that the streaming URL will be valid, in seconds. Specify a value between 1 and 604800 seconds. The default is 3600 seconds.
cibsuValidity :: Lens' CreateImageBuilderStreamingURL (Maybe Integer)
cibsuValidity = lens _cibsuValidity (\ s a -> s{_cibsuValidity = a})

-- | The name of the image builder.
cibsuName :: Lens' CreateImageBuilderStreamingURL Text
cibsuName = lens _cibsuName (\ s a -> s{_cibsuName = a})

instance AWSRequest CreateImageBuilderStreamingURL
         where
        type Rs CreateImageBuilderStreamingURL =
             CreateImageBuilderStreamingURLResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateImageBuilderStreamingURLResponse' <$>
                   (x .?> "StreamingURL") <*> (x .?> "Expires") <*>
                     (pure (fromEnum s)))

instance Hashable CreateImageBuilderStreamingURL
         where

instance NFData CreateImageBuilderStreamingURL where

instance ToHeaders CreateImageBuilderStreamingURL
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CreateImageBuilderStreamingURL"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateImageBuilderStreamingURL where
        toJSON CreateImageBuilderStreamingURL'{..}
          = object
              (catMaybes
                 [("Validity" .=) <$> _cibsuValidity,
                  Just ("Name" .= _cibsuName)])

instance ToPath CreateImageBuilderStreamingURL where
        toPath = const "/"

instance ToQuery CreateImageBuilderStreamingURL where
        toQuery = const mempty

-- | /See:/ 'createImageBuilderStreamingURLResponse' smart constructor.
data CreateImageBuilderStreamingURLResponse = CreateImageBuilderStreamingURLResponse'
  { _cibsursStreamingURL   :: !(Maybe Text)
  , _cibsursExpires        :: !(Maybe POSIX)
  , _cibsursResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateImageBuilderStreamingURLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibsursStreamingURL' - The URL to start the AppStream 2.0 streaming session.
--
-- * 'cibsursExpires' - The elapsed time, in seconds after the Unix epoch, when this URL expires.
--
-- * 'cibsursResponseStatus' - -- | The response status code.
createImageBuilderStreamingURLResponse
    :: Int -- ^ 'cibsursResponseStatus'
    -> CreateImageBuilderStreamingURLResponse
createImageBuilderStreamingURLResponse pResponseStatus_ =
  CreateImageBuilderStreamingURLResponse'
    { _cibsursStreamingURL = Nothing
    , _cibsursExpires = Nothing
    , _cibsursResponseStatus = pResponseStatus_
    }


-- | The URL to start the AppStream 2.0 streaming session.
cibsursStreamingURL :: Lens' CreateImageBuilderStreamingURLResponse (Maybe Text)
cibsursStreamingURL = lens _cibsursStreamingURL (\ s a -> s{_cibsursStreamingURL = a})

-- | The elapsed time, in seconds after the Unix epoch, when this URL expires.
cibsursExpires :: Lens' CreateImageBuilderStreamingURLResponse (Maybe UTCTime)
cibsursExpires = lens _cibsursExpires (\ s a -> s{_cibsursExpires = a}) . mapping _Time

-- | -- | The response status code.
cibsursResponseStatus :: Lens' CreateImageBuilderStreamingURLResponse Int
cibsursResponseStatus = lens _cibsursResponseStatus (\ s a -> s{_cibsursResponseStatus = a})

instance NFData
           CreateImageBuilderStreamingURLResponse
         where
