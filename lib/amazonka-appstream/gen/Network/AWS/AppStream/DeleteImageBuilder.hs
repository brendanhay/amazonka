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
-- Module      : Network.AWS.AppStream.DeleteImageBuilder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified image builder and releases the capacity.
--
--
module Network.AWS.AppStream.DeleteImageBuilder
    (
    -- * Creating a Request
      deleteImageBuilder
    , DeleteImageBuilder
    -- * Request Lenses
    , dibName

    -- * Destructuring the Response
    , deleteImageBuilderResponse
    , DeleteImageBuilderResponse
    -- * Response Lenses
    , dibrsImageBuilder
    , dibrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteImageBuilder' smart constructor.
newtype DeleteImageBuilder = DeleteImageBuilder'
  { _dibName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteImageBuilder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dibName' - The name of the image builder.
deleteImageBuilder
    :: Text -- ^ 'dibName'
    -> DeleteImageBuilder
deleteImageBuilder pName_ = DeleteImageBuilder' {_dibName = pName_}


-- | The name of the image builder.
dibName :: Lens' DeleteImageBuilder Text
dibName = lens _dibName (\ s a -> s{_dibName = a})

instance AWSRequest DeleteImageBuilder where
        type Rs DeleteImageBuilder =
             DeleteImageBuilderResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DeleteImageBuilderResponse' <$>
                   (x .?> "ImageBuilder") <*> (pure (fromEnum s)))

instance Hashable DeleteImageBuilder where

instance NFData DeleteImageBuilder where

instance ToHeaders DeleteImageBuilder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DeleteImageBuilder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteImageBuilder where
        toJSON DeleteImageBuilder'{..}
          = object (catMaybes [Just ("Name" .= _dibName)])

instance ToPath DeleteImageBuilder where
        toPath = const "/"

instance ToQuery DeleteImageBuilder where
        toQuery = const mempty

-- | /See:/ 'deleteImageBuilderResponse' smart constructor.
data DeleteImageBuilderResponse = DeleteImageBuilderResponse'
  { _dibrsImageBuilder   :: !(Maybe ImageBuilder)
  , _dibrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteImageBuilderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dibrsImageBuilder' - Information about the image builder.
--
-- * 'dibrsResponseStatus' - -- | The response status code.
deleteImageBuilderResponse
    :: Int -- ^ 'dibrsResponseStatus'
    -> DeleteImageBuilderResponse
deleteImageBuilderResponse pResponseStatus_ =
  DeleteImageBuilderResponse'
    {_dibrsImageBuilder = Nothing, _dibrsResponseStatus = pResponseStatus_}


-- | Information about the image builder.
dibrsImageBuilder :: Lens' DeleteImageBuilderResponse (Maybe ImageBuilder)
dibrsImageBuilder = lens _dibrsImageBuilder (\ s a -> s{_dibrsImageBuilder = a})

-- | -- | The response status code.
dibrsResponseStatus :: Lens' DeleteImageBuilderResponse Int
dibrsResponseStatus = lens _dibrsResponseStatus (\ s a -> s{_dibrsResponseStatus = a})

instance NFData DeleteImageBuilderResponse where
