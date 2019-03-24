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
-- Module      : Network.AWS.Lambda.PublishLayerVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> from a ZIP archive. Each time you call @PublishLayerVersion@ with the same version name, a new version is created.
--
--
-- Add layers to your function with 'CreateFunction' or 'UpdateFunctionConfiguration' .
--
module Network.AWS.Lambda.PublishLayerVersion
    (
    -- * Creating a Request
      publishLayerVersion
    , PublishLayerVersion
    -- * Request Lenses
    , plvLicenseInfo
    , plvDescription
    , plvCompatibleRuntimes
    , plvLayerName
    , plvContent

    -- * Destructuring the Response
    , publishLayerVersionResponse
    , PublishLayerVersionResponse
    -- * Response Lenses
    , plvrsLayerVersionARN
    , plvrsContent
    , plvrsCreatedDate
    , plvrsVersion
    , plvrsLicenseInfo
    , plvrsLayerARN
    , plvrsDescription
    , plvrsCompatibleRuntimes
    , plvrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'publishLayerVersion' smart constructor.
data PublishLayerVersion = PublishLayerVersion'
  { _plvLicenseInfo        :: !(Maybe Text)
  , _plvDescription        :: !(Maybe Text)
  , _plvCompatibleRuntimes :: !(Maybe [Runtime])
  , _plvLayerName          :: !Text
  , _plvContent            :: !LayerVersionContentInput
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublishLayerVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plvLicenseInfo' - The layer's software license. It can be any of the following:     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .     * The full text of the license.
--
-- * 'plvDescription' - The description of the version.
--
-- * 'plvCompatibleRuntimes' - A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
--
-- * 'plvLayerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- * 'plvContent' - The function layer archive.
publishLayerVersion
    :: Text -- ^ 'plvLayerName'
    -> LayerVersionContentInput -- ^ 'plvContent'
    -> PublishLayerVersion
publishLayerVersion pLayerName_ pContent_ =
  PublishLayerVersion'
    { _plvLicenseInfo = Nothing
    , _plvDescription = Nothing
    , _plvCompatibleRuntimes = Nothing
    , _plvLayerName = pLayerName_
    , _plvContent = pContent_
    }


-- | The layer's software license. It can be any of the following:     * An <https://spdx.org/licenses/ SPDX license identifier> . For example, @MIT@ .     * The URL of a license hosted on the internet. For example, @https://opensource.org/licenses/MIT@ .     * The full text of the license.
plvLicenseInfo :: Lens' PublishLayerVersion (Maybe Text)
plvLicenseInfo = lens _plvLicenseInfo (\ s a -> s{_plvLicenseInfo = a})

-- | The description of the version.
plvDescription :: Lens' PublishLayerVersion (Maybe Text)
plvDescription = lens _plvDescription (\ s a -> s{_plvDescription = a})

-- | A list of compatible <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html function runtimes> . Used for filtering with 'ListLayers' and 'ListLayerVersions' .
plvCompatibleRuntimes :: Lens' PublishLayerVersion [Runtime]
plvCompatibleRuntimes = lens _plvCompatibleRuntimes (\ s a -> s{_plvCompatibleRuntimes = a}) . _Default . _Coerce

-- | The name or Amazon Resource Name (ARN) of the layer.
plvLayerName :: Lens' PublishLayerVersion Text
plvLayerName = lens _plvLayerName (\ s a -> s{_plvLayerName = a})

-- | The function layer archive.
plvContent :: Lens' PublishLayerVersion LayerVersionContentInput
plvContent = lens _plvContent (\ s a -> s{_plvContent = a})

instance AWSRequest PublishLayerVersion where
        type Rs PublishLayerVersion =
             PublishLayerVersionResponse
        request = postJSON lambda
        response
          = receiveJSON
              (\ s h x ->
                 PublishLayerVersionResponse' <$>
                   (x .?> "LayerVersionArn") <*> (x .?> "Content") <*>
                     (x .?> "CreatedDate")
                     <*> (x .?> "Version")
                     <*> (x .?> "LicenseInfo")
                     <*> (x .?> "LayerArn")
                     <*> (x .?> "Description")
                     <*> (x .?> "CompatibleRuntimes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable PublishLayerVersion where

instance NFData PublishLayerVersion where

instance ToHeaders PublishLayerVersion where
        toHeaders = const mempty

instance ToJSON PublishLayerVersion where
        toJSON PublishLayerVersion'{..}
          = object
              (catMaybes
                 [("LicenseInfo" .=) <$> _plvLicenseInfo,
                  ("Description" .=) <$> _plvDescription,
                  ("CompatibleRuntimes" .=) <$> _plvCompatibleRuntimes,
                  Just ("Content" .= _plvContent)])

instance ToPath PublishLayerVersion where
        toPath PublishLayerVersion'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _plvLayerName,
               "/versions"]

instance ToQuery PublishLayerVersion where
        toQuery = const mempty

-- | /See:/ 'publishLayerVersionResponse' smart constructor.
data PublishLayerVersionResponse = PublishLayerVersionResponse'
  { _plvrsLayerVersionARN    :: !(Maybe Text)
  , _plvrsContent            :: !(Maybe LayerVersionContentOutput)
  , _plvrsCreatedDate        :: !(Maybe Text)
  , _plvrsVersion            :: !(Maybe Integer)
  , _plvrsLicenseInfo        :: !(Maybe Text)
  , _plvrsLayerARN           :: !(Maybe Text)
  , _plvrsDescription        :: !(Maybe Text)
  , _plvrsCompatibleRuntimes :: !(Maybe [Runtime])
  , _plvrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublishLayerVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'plvrsLayerVersionARN' - The ARN of the layer version.
--
-- * 'plvrsContent' - Details about the layer version.
--
-- * 'plvrsCreatedDate' - The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- * 'plvrsVersion' - The version number.
--
-- * 'plvrsLicenseInfo' - The layer's software license.
--
-- * 'plvrsLayerARN' - The ARN of the layer.
--
-- * 'plvrsDescription' - The description of the version.
--
-- * 'plvrsCompatibleRuntimes' - The layer's compatible runtimes.
--
-- * 'plvrsResponseStatus' - -- | The response status code.
publishLayerVersionResponse
    :: Int -- ^ 'plvrsResponseStatus'
    -> PublishLayerVersionResponse
publishLayerVersionResponse pResponseStatus_ =
  PublishLayerVersionResponse'
    { _plvrsLayerVersionARN = Nothing
    , _plvrsContent = Nothing
    , _plvrsCreatedDate = Nothing
    , _plvrsVersion = Nothing
    , _plvrsLicenseInfo = Nothing
    , _plvrsLayerARN = Nothing
    , _plvrsDescription = Nothing
    , _plvrsCompatibleRuntimes = Nothing
    , _plvrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the layer version.
plvrsLayerVersionARN :: Lens' PublishLayerVersionResponse (Maybe Text)
plvrsLayerVersionARN = lens _plvrsLayerVersionARN (\ s a -> s{_plvrsLayerVersionARN = a})

-- | Details about the layer version.
plvrsContent :: Lens' PublishLayerVersionResponse (Maybe LayerVersionContentOutput)
plvrsContent = lens _plvrsContent (\ s a -> s{_plvrsContent = a})

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
plvrsCreatedDate :: Lens' PublishLayerVersionResponse (Maybe Text)
plvrsCreatedDate = lens _plvrsCreatedDate (\ s a -> s{_plvrsCreatedDate = a})

-- | The version number.
plvrsVersion :: Lens' PublishLayerVersionResponse (Maybe Integer)
plvrsVersion = lens _plvrsVersion (\ s a -> s{_plvrsVersion = a})

-- | The layer's software license.
plvrsLicenseInfo :: Lens' PublishLayerVersionResponse (Maybe Text)
plvrsLicenseInfo = lens _plvrsLicenseInfo (\ s a -> s{_plvrsLicenseInfo = a})

-- | The ARN of the layer.
plvrsLayerARN :: Lens' PublishLayerVersionResponse (Maybe Text)
plvrsLayerARN = lens _plvrsLayerARN (\ s a -> s{_plvrsLayerARN = a})

-- | The description of the version.
plvrsDescription :: Lens' PublishLayerVersionResponse (Maybe Text)
plvrsDescription = lens _plvrsDescription (\ s a -> s{_plvrsDescription = a})

-- | The layer's compatible runtimes.
plvrsCompatibleRuntimes :: Lens' PublishLayerVersionResponse [Runtime]
plvrsCompatibleRuntimes = lens _plvrsCompatibleRuntimes (\ s a -> s{_plvrsCompatibleRuntimes = a}) . _Default . _Coerce

-- | -- | The response status code.
plvrsResponseStatus :: Lens' PublishLayerVersionResponse Int
plvrsResponseStatus = lens _plvrsResponseStatus (\ s a -> s{_plvrsResponseStatus = a})

instance NFData PublishLayerVersionResponse where
