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
-- Module      : Network.AWS.Lambda.GetLayerVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> , with a link to download the layer archive that's valid for 10 minutes.
--
--
module Network.AWS.Lambda.GetLayerVersion
    (
    -- * Creating a Request
      getLayerVersion
    , GetLayerVersion
    -- * Request Lenses
    , glvLayerName
    , glvVersionNumber

    -- * Destructuring the Response
    , getLayerVersionResponse
    , GetLayerVersionResponse
    -- * Response Lenses
    , glvrsLayerVersionARN
    , glvrsContent
    , glvrsCreatedDate
    , glvrsVersion
    , glvrsLicenseInfo
    , glvrsLayerARN
    , glvrsDescription
    , glvrsCompatibleRuntimes
    , glvrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLayerVersion' smart constructor.
data GetLayerVersion = GetLayerVersion'
  { _glvLayerName     :: !Text
  , _glvVersionNumber :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLayerVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glvLayerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- * 'glvVersionNumber' - The version number.
getLayerVersion
    :: Text -- ^ 'glvLayerName'
    -> Integer -- ^ 'glvVersionNumber'
    -> GetLayerVersion
getLayerVersion pLayerName_ pVersionNumber_ =
  GetLayerVersion'
    {_glvLayerName = pLayerName_, _glvVersionNumber = pVersionNumber_}


-- | The name or Amazon Resource Name (ARN) of the layer.
glvLayerName :: Lens' GetLayerVersion Text
glvLayerName = lens _glvLayerName (\ s a -> s{_glvLayerName = a})

-- | The version number.
glvVersionNumber :: Lens' GetLayerVersion Integer
glvVersionNumber = lens _glvVersionNumber (\ s a -> s{_glvVersionNumber = a})

instance AWSRequest GetLayerVersion where
        type Rs GetLayerVersion = GetLayerVersionResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 GetLayerVersionResponse' <$>
                   (x .?> "LayerVersionArn") <*> (x .?> "Content") <*>
                     (x .?> "CreatedDate")
                     <*> (x .?> "Version")
                     <*> (x .?> "LicenseInfo")
                     <*> (x .?> "LayerArn")
                     <*> (x .?> "Description")
                     <*> (x .?> "CompatibleRuntimes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetLayerVersion where

instance NFData GetLayerVersion where

instance ToHeaders GetLayerVersion where
        toHeaders = const mempty

instance ToPath GetLayerVersion where
        toPath GetLayerVersion'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _glvLayerName,
               "/versions/", toBS _glvVersionNumber]

instance ToQuery GetLayerVersion where
        toQuery = const mempty

-- | /See:/ 'getLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { _glvrsLayerVersionARN    :: !(Maybe Text)
  , _glvrsContent            :: !(Maybe LayerVersionContentOutput)
  , _glvrsCreatedDate        :: !(Maybe Text)
  , _glvrsVersion            :: !(Maybe Integer)
  , _glvrsLicenseInfo        :: !(Maybe Text)
  , _glvrsLayerARN           :: !(Maybe Text)
  , _glvrsDescription        :: !(Maybe Text)
  , _glvrsCompatibleRuntimes :: !(Maybe [Runtime])
  , _glvrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLayerVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glvrsLayerVersionARN' - The ARN of the layer version.
--
-- * 'glvrsContent' - Details about the layer version.
--
-- * 'glvrsCreatedDate' - The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- * 'glvrsVersion' - The version number.
--
-- * 'glvrsLicenseInfo' - The layer's software license.
--
-- * 'glvrsLayerARN' - The ARN of the layer.
--
-- * 'glvrsDescription' - The description of the version.
--
-- * 'glvrsCompatibleRuntimes' - The layer's compatible runtimes.
--
-- * 'glvrsResponseStatus' - -- | The response status code.
getLayerVersionResponse
    :: Int -- ^ 'glvrsResponseStatus'
    -> GetLayerVersionResponse
getLayerVersionResponse pResponseStatus_ =
  GetLayerVersionResponse'
    { _glvrsLayerVersionARN = Nothing
    , _glvrsContent = Nothing
    , _glvrsCreatedDate = Nothing
    , _glvrsVersion = Nothing
    , _glvrsLicenseInfo = Nothing
    , _glvrsLayerARN = Nothing
    , _glvrsDescription = Nothing
    , _glvrsCompatibleRuntimes = Nothing
    , _glvrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the layer version.
glvrsLayerVersionARN :: Lens' GetLayerVersionResponse (Maybe Text)
glvrsLayerVersionARN = lens _glvrsLayerVersionARN (\ s a -> s{_glvrsLayerVersionARN = a})

-- | Details about the layer version.
glvrsContent :: Lens' GetLayerVersionResponse (Maybe LayerVersionContentOutput)
glvrsContent = lens _glvrsContent (\ s a -> s{_glvrsContent = a})

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
glvrsCreatedDate :: Lens' GetLayerVersionResponse (Maybe Text)
glvrsCreatedDate = lens _glvrsCreatedDate (\ s a -> s{_glvrsCreatedDate = a})

-- | The version number.
glvrsVersion :: Lens' GetLayerVersionResponse (Maybe Integer)
glvrsVersion = lens _glvrsVersion (\ s a -> s{_glvrsVersion = a})

-- | The layer's software license.
glvrsLicenseInfo :: Lens' GetLayerVersionResponse (Maybe Text)
glvrsLicenseInfo = lens _glvrsLicenseInfo (\ s a -> s{_glvrsLicenseInfo = a})

-- | The ARN of the layer.
glvrsLayerARN :: Lens' GetLayerVersionResponse (Maybe Text)
glvrsLayerARN = lens _glvrsLayerARN (\ s a -> s{_glvrsLayerARN = a})

-- | The description of the version.
glvrsDescription :: Lens' GetLayerVersionResponse (Maybe Text)
glvrsDescription = lens _glvrsDescription (\ s a -> s{_glvrsDescription = a})

-- | The layer's compatible runtimes.
glvrsCompatibleRuntimes :: Lens' GetLayerVersionResponse [Runtime]
glvrsCompatibleRuntimes = lens _glvrsCompatibleRuntimes (\ s a -> s{_glvrsCompatibleRuntimes = a}) . _Default . _Coerce

-- | -- | The response status code.
glvrsResponseStatus :: Lens' GetLayerVersionResponse Int
glvrsResponseStatus = lens _glvrsResponseStatus (\ s a -> s{_glvrsResponseStatus = a})

instance NFData GetLayerVersionResponse where
