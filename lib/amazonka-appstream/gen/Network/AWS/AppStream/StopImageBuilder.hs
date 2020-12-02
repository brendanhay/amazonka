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
-- Module      : Network.AWS.AppStream.StopImageBuilder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified image builder.
--
--
module Network.AWS.AppStream.StopImageBuilder
    (
    -- * Creating a Request
      stopImageBuilder
    , StopImageBuilder
    -- * Request Lenses
    , stoName

    -- * Destructuring the Response
    , stopImageBuilderResponse
    , StopImageBuilderResponse
    -- * Response Lenses
    , sibrsImageBuilder
    , sibrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopImageBuilder' smart constructor.
newtype StopImageBuilder = StopImageBuilder'
  { _stoName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopImageBuilder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stoName' - The name of the image builder.
stopImageBuilder
    :: Text -- ^ 'stoName'
    -> StopImageBuilder
stopImageBuilder pName_ = StopImageBuilder' {_stoName = pName_}


-- | The name of the image builder.
stoName :: Lens' StopImageBuilder Text
stoName = lens _stoName (\ s a -> s{_stoName = a})

instance AWSRequest StopImageBuilder where
        type Rs StopImageBuilder = StopImageBuilderResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 StopImageBuilderResponse' <$>
                   (x .?> "ImageBuilder") <*> (pure (fromEnum s)))

instance Hashable StopImageBuilder where

instance NFData StopImageBuilder where

instance ToHeaders StopImageBuilder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.StopImageBuilder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopImageBuilder where
        toJSON StopImageBuilder'{..}
          = object (catMaybes [Just ("Name" .= _stoName)])

instance ToPath StopImageBuilder where
        toPath = const "/"

instance ToQuery StopImageBuilder where
        toQuery = const mempty

-- | /See:/ 'stopImageBuilderResponse' smart constructor.
data StopImageBuilderResponse = StopImageBuilderResponse'
  { _sibrsImageBuilder   :: !(Maybe ImageBuilder)
  , _sibrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopImageBuilderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sibrsImageBuilder' - Information about the image builder.
--
-- * 'sibrsResponseStatus' - -- | The response status code.
stopImageBuilderResponse
    :: Int -- ^ 'sibrsResponseStatus'
    -> StopImageBuilderResponse
stopImageBuilderResponse pResponseStatus_ =
  StopImageBuilderResponse'
    {_sibrsImageBuilder = Nothing, _sibrsResponseStatus = pResponseStatus_}


-- | Information about the image builder.
sibrsImageBuilder :: Lens' StopImageBuilderResponse (Maybe ImageBuilder)
sibrsImageBuilder = lens _sibrsImageBuilder (\ s a -> s{_sibrsImageBuilder = a})

-- | -- | The response status code.
sibrsResponseStatus :: Lens' StopImageBuilderResponse Int
sibrsResponseStatus = lens _sibrsResponseStatus (\ s a -> s{_sibrsResponseStatus = a})

instance NFData StopImageBuilderResponse where
