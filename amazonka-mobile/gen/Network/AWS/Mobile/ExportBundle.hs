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
-- Module      : Network.AWS.Mobile.ExportBundle
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates customized software development kit (SDK) and or tool packages used to integrate mobile web or mobile app clients with backend AWS resources.
--
--
module Network.AWS.Mobile.ExportBundle
    (
    -- * Creating a Request
      exportBundle
    , ExportBundle
    -- * Request Lenses
    , ebPlatform
    , ebProjectId
    , ebBundleId

    -- * Destructuring the Response
    , exportBundleResponse
    , ExportBundleResponse
    -- * Response Lenses
    , ebrsDownloadURL
    , ebrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used to request generation of custom SDK and tool packages required to integrate mobile web or app clients with backed AWS resources.
--
--
--
-- /See:/ 'exportBundle' smart constructor.
data ExportBundle = ExportBundle'
  { _ebPlatform  :: !(Maybe Platform)
  , _ebProjectId :: !(Maybe Text)
  , _ebBundleId  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportBundle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebPlatform' - Developer desktop or target application platform.
--
-- * 'ebProjectId' - Unique project identifier.
--
-- * 'ebBundleId' - Unique bundle identifier.
exportBundle
    :: Text -- ^ 'ebBundleId'
    -> ExportBundle
exportBundle pBundleId_ =
  ExportBundle'
    {_ebPlatform = Nothing, _ebProjectId = Nothing, _ebBundleId = pBundleId_}


-- | Developer desktop or target application platform.
ebPlatform :: Lens' ExportBundle (Maybe Platform)
ebPlatform = lens _ebPlatform (\ s a -> s{_ebPlatform = a})

-- | Unique project identifier.
ebProjectId :: Lens' ExportBundle (Maybe Text)
ebProjectId = lens _ebProjectId (\ s a -> s{_ebProjectId = a})

-- | Unique bundle identifier.
ebBundleId :: Lens' ExportBundle Text
ebBundleId = lens _ebBundleId (\ s a -> s{_ebBundleId = a})

instance AWSRequest ExportBundle where
        type Rs ExportBundle = ExportBundleResponse
        request = postJSON mobile
        response
          = receiveJSON
              (\ s h x ->
                 ExportBundleResponse' <$>
                   (x .?> "downloadUrl") <*> (pure (fromEnum s)))

instance Hashable ExportBundle where

instance NFData ExportBundle where

instance ToHeaders ExportBundle where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ExportBundle where
        toJSON = const (Object mempty)

instance ToPath ExportBundle where
        toPath ExportBundle'{..}
          = mconcat ["/bundles/", toBS _ebBundleId]

instance ToQuery ExportBundle where
        toQuery ExportBundle'{..}
          = mconcat
              ["platform" =: _ebPlatform,
               "projectId" =: _ebProjectId]

-- | Result structure which contains link to download custom-generated SDK and tool packages used to integrate mobile web or app clients with backed AWS resources.
--
--
--
-- /See:/ 'exportBundleResponse' smart constructor.
data ExportBundleResponse = ExportBundleResponse'
  { _ebrsDownloadURL    :: !(Maybe Text)
  , _ebrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportBundleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ebrsDownloadURL' - URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
--
-- * 'ebrsResponseStatus' - -- | The response status code.
exportBundleResponse
    :: Int -- ^ 'ebrsResponseStatus'
    -> ExportBundleResponse
exportBundleResponse pResponseStatus_ =
  ExportBundleResponse'
    {_ebrsDownloadURL = Nothing, _ebrsResponseStatus = pResponseStatus_}


-- | URL which contains the custom-generated SDK and tool packages used to integrate the client mobile app or web app with the AWS resources created by the AWS Mobile Hub project.
ebrsDownloadURL :: Lens' ExportBundleResponse (Maybe Text)
ebrsDownloadURL = lens _ebrsDownloadURL (\ s a -> s{_ebrsDownloadURL = a})

-- | -- | The response status code.
ebrsResponseStatus :: Lens' ExportBundleResponse Int
ebrsResponseStatus = lens _ebrsResponseStatus (\ s a -> s{_ebrsResponseStatus = a})

instance NFData ExportBundleResponse where
