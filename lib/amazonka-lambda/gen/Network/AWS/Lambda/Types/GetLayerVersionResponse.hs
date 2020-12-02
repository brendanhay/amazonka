{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.GetLayerVersionResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.GetLayerVersionResponse where

import Network.AWS.Lambda.Types.LayerVersionContentOutput
import Network.AWS.Lambda.Types.Runtime
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'getLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { _glvLayerVersionARN ::
      !(Maybe Text),
    _glvContent ::
      !(Maybe LayerVersionContentOutput),
    _glvCreatedDate :: !(Maybe Text),
    _glvVersion :: !(Maybe Integer),
    _glvLicenseInfo :: !(Maybe Text),
    _glvLayerARN :: !(Maybe Text),
    _glvDescription :: !(Maybe Text),
    _glvCompatibleRuntimes ::
      !(Maybe [Runtime])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetLayerVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glvLayerVersionARN' - The ARN of the layer version.
--
-- * 'glvContent' - Details about the layer version.
--
-- * 'glvCreatedDate' - The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- * 'glvVersion' - The version number.
--
-- * 'glvLicenseInfo' - The layer's software license.
--
-- * 'glvLayerARN' - The ARN of the layer.
--
-- * 'glvDescription' - The description of the version.
--
-- * 'glvCompatibleRuntimes' - The layer's compatible runtimes.
getLayerVersionResponse ::
  GetLayerVersionResponse
getLayerVersionResponse =
  GetLayerVersionResponse'
    { _glvLayerVersionARN = Nothing,
      _glvContent = Nothing,
      _glvCreatedDate = Nothing,
      _glvVersion = Nothing,
      _glvLicenseInfo = Nothing,
      _glvLayerARN = Nothing,
      _glvDescription = Nothing,
      _glvCompatibleRuntimes = Nothing
    }

-- | The ARN of the layer version.
glvLayerVersionARN :: Lens' GetLayerVersionResponse (Maybe Text)
glvLayerVersionARN = lens _glvLayerVersionARN (\s a -> s {_glvLayerVersionARN = a})

-- | Details about the layer version.
glvContent :: Lens' GetLayerVersionResponse (Maybe LayerVersionContentOutput)
glvContent = lens _glvContent (\s a -> s {_glvContent = a})

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
glvCreatedDate :: Lens' GetLayerVersionResponse (Maybe Text)
glvCreatedDate = lens _glvCreatedDate (\s a -> s {_glvCreatedDate = a})

-- | The version number.
glvVersion :: Lens' GetLayerVersionResponse (Maybe Integer)
glvVersion = lens _glvVersion (\s a -> s {_glvVersion = a})

-- | The layer's software license.
glvLicenseInfo :: Lens' GetLayerVersionResponse (Maybe Text)
glvLicenseInfo = lens _glvLicenseInfo (\s a -> s {_glvLicenseInfo = a})

-- | The ARN of the layer.
glvLayerARN :: Lens' GetLayerVersionResponse (Maybe Text)
glvLayerARN = lens _glvLayerARN (\s a -> s {_glvLayerARN = a})

-- | The description of the version.
glvDescription :: Lens' GetLayerVersionResponse (Maybe Text)
glvDescription = lens _glvDescription (\s a -> s {_glvDescription = a})

-- | The layer's compatible runtimes.
glvCompatibleRuntimes :: Lens' GetLayerVersionResponse [Runtime]
glvCompatibleRuntimes = lens _glvCompatibleRuntimes (\s a -> s {_glvCompatibleRuntimes = a}) . _Default . _Coerce

instance FromJSON GetLayerVersionResponse where
  parseJSON =
    withObject
      "GetLayerVersionResponse"
      ( \x ->
          GetLayerVersionResponse'
            <$> (x .:? "LayerVersionArn")
            <*> (x .:? "Content")
            <*> (x .:? "CreatedDate")
            <*> (x .:? "Version")
            <*> (x .:? "LicenseInfo")
            <*> (x .:? "LayerArn")
            <*> (x .:? "Description")
            <*> (x .:? "CompatibleRuntimes" .!= mempty)
      )

instance Hashable GetLayerVersionResponse

instance NFData GetLayerVersionResponse
