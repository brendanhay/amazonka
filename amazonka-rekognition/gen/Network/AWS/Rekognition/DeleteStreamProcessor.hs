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
-- Module      : Network.AWS.Rekognition.DeleteStreamProcessor
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the stream processor identified by @Name@ . You assign the value for @Name@ when you create the stream processor with . You might not be able to use the same name for a stream processor for a few seconds after calling @DeleteStreamProcessor@ .
--
--
module Network.AWS.Rekognition.DeleteStreamProcessor
    (
    -- * Creating a Request
      deleteStreamProcessor
    , DeleteStreamProcessor
    -- * Request Lenses
    , dName

    -- * Destructuring the Response
    , deleteStreamProcessorResponse
    , DeleteStreamProcessorResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStreamProcessor' smart constructor.
newtype DeleteStreamProcessor = DeleteStreamProcessor'
  { _dName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStreamProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The name of the stream processor you want to delete.
deleteStreamProcessor
    :: Text -- ^ 'dName'
    -> DeleteStreamProcessor
deleteStreamProcessor pName_ = DeleteStreamProcessor' {_dName = pName_}


-- | The name of the stream processor you want to delete.
dName :: Lens' DeleteStreamProcessor Text
dName = lens _dName (\ s a -> s{_dName = a})

instance AWSRequest DeleteStreamProcessor where
        type Rs DeleteStreamProcessor =
             DeleteStreamProcessorResponse
        request = postJSON rekognition
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteStreamProcessorResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteStreamProcessor where

instance NFData DeleteStreamProcessor where

instance ToHeaders DeleteStreamProcessor where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.DeleteStreamProcessor" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteStreamProcessor where
        toJSON DeleteStreamProcessor'{..}
          = object (catMaybes [Just ("Name" .= _dName)])

instance ToPath DeleteStreamProcessor where
        toPath = const "/"

instance ToQuery DeleteStreamProcessor where
        toQuery = const mempty

-- | /See:/ 'deleteStreamProcessorResponse' smart constructor.
newtype DeleteStreamProcessorResponse = DeleteStreamProcessorResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStreamProcessorResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteStreamProcessorResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteStreamProcessorResponse
deleteStreamProcessorResponse pResponseStatus_ =
  DeleteStreamProcessorResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteStreamProcessorResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteStreamProcessorResponse where
