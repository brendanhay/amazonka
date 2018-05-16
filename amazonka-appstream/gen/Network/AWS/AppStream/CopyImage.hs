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
-- Module      : Network.AWS.AppStream.CopyImage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the image within the same region or to a new region within the same AWS account. Note that any tags you added to the image will not be copied.
--
--
module Network.AWS.AppStream.CopyImage
    (
    -- * Creating a Request
      copyImage
    , CopyImage
    -- * Request Lenses
    , ciDestinationImageDescription
    , ciSourceImageName
    , ciDestinationImageName
    , ciDestinationRegion

    -- * Destructuring the Response
    , copyImageResponse
    , CopyImageResponse
    -- * Response Lenses
    , cirsDestinationImageName
    , cirsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'copyImage' smart constructor.
data CopyImage = CopyImage'
  { _ciDestinationImageDescription :: !(Maybe Text)
  , _ciSourceImageName             :: !Text
  , _ciDestinationImageName        :: !Text
  , _ciDestinationRegion           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciDestinationImageDescription' - The description that the image will have when it is copied to the destination.
--
-- * 'ciSourceImageName' - The name of the image to copy.
--
-- * 'ciDestinationImageName' - The name that the image will have when it is copied to the destination.
--
-- * 'ciDestinationRegion' - The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
copyImage
    :: Text -- ^ 'ciSourceImageName'
    -> Text -- ^ 'ciDestinationImageName'
    -> Text -- ^ 'ciDestinationRegion'
    -> CopyImage
copyImage pSourceImageName_ pDestinationImageName_ pDestinationRegion_ =
  CopyImage'
    { _ciDestinationImageDescription = Nothing
    , _ciSourceImageName = pSourceImageName_
    , _ciDestinationImageName = pDestinationImageName_
    , _ciDestinationRegion = pDestinationRegion_
    }


-- | The description that the image will have when it is copied to the destination.
ciDestinationImageDescription :: Lens' CopyImage (Maybe Text)
ciDestinationImageDescription = lens _ciDestinationImageDescription (\ s a -> s{_ciDestinationImageDescription = a})

-- | The name of the image to copy.
ciSourceImageName :: Lens' CopyImage Text
ciSourceImageName = lens _ciSourceImageName (\ s a -> s{_ciSourceImageName = a})

-- | The name that the image will have when it is copied to the destination.
ciDestinationImageName :: Lens' CopyImage Text
ciDestinationImageName = lens _ciDestinationImageName (\ s a -> s{_ciDestinationImageName = a})

-- | The destination region to which the image will be copied. This parameter is required, even if you are copying an image within the same region.
ciDestinationRegion :: Lens' CopyImage Text
ciDestinationRegion = lens _ciDestinationRegion (\ s a -> s{_ciDestinationRegion = a})

instance AWSRequest CopyImage where
        type Rs CopyImage = CopyImageResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CopyImageResponse' <$>
                   (x .?> "DestinationImageName") <*>
                     (pure (fromEnum s)))

instance Hashable CopyImage where

instance NFData CopyImage where

instance ToHeaders CopyImage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CopyImage" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CopyImage where
        toJSON CopyImage'{..}
          = object
              (catMaybes
                 [("DestinationImageDescription" .=) <$>
                    _ciDestinationImageDescription,
                  Just ("SourceImageName" .= _ciSourceImageName),
                  Just
                    ("DestinationImageName" .= _ciDestinationImageName),
                  Just ("DestinationRegion" .= _ciDestinationRegion)])

instance ToPath CopyImage where
        toPath = const "/"

instance ToQuery CopyImage where
        toQuery = const mempty

-- | /See:/ 'copyImageResponse' smart constructor.
data CopyImageResponse = CopyImageResponse'
  { _cirsDestinationImageName :: !(Maybe Text)
  , _cirsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CopyImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirsDestinationImageName' - The name of the destination image.
--
-- * 'cirsResponseStatus' - -- | The response status code.
copyImageResponse
    :: Int -- ^ 'cirsResponseStatus'
    -> CopyImageResponse
copyImageResponse pResponseStatus_ =
  CopyImageResponse'
    { _cirsDestinationImageName = Nothing
    , _cirsResponseStatus = pResponseStatus_
    }


-- | The name of the destination image.
cirsDestinationImageName :: Lens' CopyImageResponse (Maybe Text)
cirsDestinationImageName = lens _cirsDestinationImageName (\ s a -> s{_cirsDestinationImageName = a})

-- | -- | The response status code.
cirsResponseStatus :: Lens' CopyImageResponse Int
cirsResponseStatus = lens _cirsResponseStatus (\ s a -> s{_cirsResponseStatus = a})

instance NFData CopyImageResponse where
