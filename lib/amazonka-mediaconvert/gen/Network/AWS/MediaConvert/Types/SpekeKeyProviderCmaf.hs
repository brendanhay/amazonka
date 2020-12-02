{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SpekeKeyProviderCmaf where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | If your output group type is CMAF, use these settings when doing DRM encryption with a SPEKE-compliant key provider. If your output group type is HLS, DASH, or Microsoft Smooth, use the SpekeKeyProvider settings instead.
--
-- /See:/ 'spekeKeyProviderCmaf' smart constructor.
data SpekeKeyProviderCmaf = SpekeKeyProviderCmaf'
  { _skpcResourceId ::
      !(Maybe Text),
    _skpcDashSignaledSystemIds :: !(Maybe [Text]),
    _skpcCertificateARN :: !(Maybe Text),
    _skpcURL :: !(Maybe Text),
    _skpcHlsSignaledSystemIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpekeKeyProviderCmaf' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'skpcResourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
--
-- * 'skpcDashSignaledSystemIds' - Specify the DRM system IDs that you want signaled in the DASH manifest that MediaConvert creates as part of this CMAF package. The DASH manifest can currently signal up to three system IDs. For more information, see https://dashif.org/identifiers/content_protection/.
--
-- * 'skpcCertificateARN' - If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
--
-- * 'skpcURL' - Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
--
-- * 'skpcHlsSignaledSystemIds' - Specify the DRM system ID that you want signaled in the HLS manifest that MediaConvert creates as part of this CMAF package. The HLS manifest can currently signal only one system ID. For more information, see https://dashif.org/identifiers/content_protection/.
spekeKeyProviderCmaf ::
  SpekeKeyProviderCmaf
spekeKeyProviderCmaf =
  SpekeKeyProviderCmaf'
    { _skpcResourceId = Nothing,
      _skpcDashSignaledSystemIds = Nothing,
      _skpcCertificateARN = Nothing,
      _skpcURL = Nothing,
      _skpcHlsSignaledSystemIds = Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
skpcResourceId :: Lens' SpekeKeyProviderCmaf (Maybe Text)
skpcResourceId = lens _skpcResourceId (\s a -> s {_skpcResourceId = a})

-- | Specify the DRM system IDs that you want signaled in the DASH manifest that MediaConvert creates as part of this CMAF package. The DASH manifest can currently signal up to three system IDs. For more information, see https://dashif.org/identifiers/content_protection/.
skpcDashSignaledSystemIds :: Lens' SpekeKeyProviderCmaf [Text]
skpcDashSignaledSystemIds = lens _skpcDashSignaledSystemIds (\s a -> s {_skpcDashSignaledSystemIds = a}) . _Default . _Coerce

-- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
skpcCertificateARN :: Lens' SpekeKeyProviderCmaf (Maybe Text)
skpcCertificateARN = lens _skpcCertificateARN (\s a -> s {_skpcCertificateARN = a})

-- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
skpcURL :: Lens' SpekeKeyProviderCmaf (Maybe Text)
skpcURL = lens _skpcURL (\s a -> s {_skpcURL = a})

-- | Specify the DRM system ID that you want signaled in the HLS manifest that MediaConvert creates as part of this CMAF package. The HLS manifest can currently signal only one system ID. For more information, see https://dashif.org/identifiers/content_protection/.
skpcHlsSignaledSystemIds :: Lens' SpekeKeyProviderCmaf [Text]
skpcHlsSignaledSystemIds = lens _skpcHlsSignaledSystemIds (\s a -> s {_skpcHlsSignaledSystemIds = a}) . _Default . _Coerce

instance FromJSON SpekeKeyProviderCmaf where
  parseJSON =
    withObject
      "SpekeKeyProviderCmaf"
      ( \x ->
          SpekeKeyProviderCmaf'
            <$> (x .:? "resourceId")
            <*> (x .:? "dashSignaledSystemIds" .!= mempty)
            <*> (x .:? "certificateArn")
            <*> (x .:? "url")
            <*> (x .:? "hlsSignaledSystemIds" .!= mempty)
      )

instance Hashable SpekeKeyProviderCmaf

instance NFData SpekeKeyProviderCmaf

instance ToJSON SpekeKeyProviderCmaf where
  toJSON SpekeKeyProviderCmaf' {..} =
    object
      ( catMaybes
          [ ("resourceId" .=) <$> _skpcResourceId,
            ("dashSignaledSystemIds" .=) <$> _skpcDashSignaledSystemIds,
            ("certificateArn" .=) <$> _skpcCertificateARN,
            ("url" .=) <$> _skpcURL,
            ("hlsSignaledSystemIds" .=) <$> _skpcHlsSignaledSystemIds
          ]
      )
