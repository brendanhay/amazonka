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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
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
-- * 'cibsuValidity' - Undocumented member.
--
-- * 'cibsuName' - Undocumented member.
createImageBuilderStreamingURL
    :: Text -- ^ 'cibsuName'
    -> CreateImageBuilderStreamingURL
createImageBuilderStreamingURL pName_ =
  CreateImageBuilderStreamingURL'
  {_cibsuValidity = Nothing, _cibsuName = pName_}


-- | Undocumented member.
cibsuValidity :: Lens' CreateImageBuilderStreamingURL (Maybe Integer)
cibsuValidity = lens _cibsuValidity (\ s a -> s{_cibsuValidity = a});

-- | Undocumented member.
cibsuName :: Lens' CreateImageBuilderStreamingURL Text
cibsuName = lens _cibsuName (\ s a -> s{_cibsuName = a});

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
-- * 'cibsursStreamingURL' - Undocumented member.
--
-- * 'cibsursExpires' - Undocumented member.
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


-- | Undocumented member.
cibsursStreamingURL :: Lens' CreateImageBuilderStreamingURLResponse (Maybe Text)
cibsursStreamingURL = lens _cibsursStreamingURL (\ s a -> s{_cibsursStreamingURL = a});

-- | Undocumented member.
cibsursExpires :: Lens' CreateImageBuilderStreamingURLResponse (Maybe UTCTime)
cibsursExpires = lens _cibsursExpires (\ s a -> s{_cibsursExpires = a}) . mapping _Time;

-- | -- | The response status code.
cibsursResponseStatus :: Lens' CreateImageBuilderStreamingURLResponse Int
cibsursResponseStatus = lens _cibsursResponseStatus (\ s a -> s{_cibsursResponseStatus = a});

instance NFData
           CreateImageBuilderStreamingURLResponse
         where
