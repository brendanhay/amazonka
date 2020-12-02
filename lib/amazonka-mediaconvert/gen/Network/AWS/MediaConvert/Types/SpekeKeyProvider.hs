{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.SpekeKeyProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.SpekeKeyProvider where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /See:/ 'spekeKeyProvider' smart constructor.
data SpekeKeyProvider = SpekeKeyProvider'
  { _sResourceId ::
      !(Maybe Text),
    _sCertificateARN :: !(Maybe Text),
    _sURL :: !(Maybe Text),
    _sSystemIds :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SpekeKeyProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sResourceId' - Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
--
-- * 'sCertificateARN' - If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
--
-- * 'sURL' - Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
--
-- * 'sSystemIds' - Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id. See  https://dashif.org/identifiers/content_protection/ for more details.
spekeKeyProvider ::
  SpekeKeyProvider
spekeKeyProvider =
  SpekeKeyProvider'
    { _sResourceId = Nothing,
      _sCertificateARN = Nothing,
      _sURL = Nothing,
      _sSystemIds = Nothing
    }

-- | Specify the resource ID that your SPEKE-compliant key provider uses to identify this content.
sResourceId :: Lens' SpekeKeyProvider (Maybe Text)
sResourceId = lens _sResourceId (\s a -> s {_sResourceId = a})

-- | If you want your key provider to encrypt the content keys that it provides to MediaConvert, set up a certificate with a master key using AWS Certificate Manager. Specify the certificate's Amazon Resource Name (ARN) here.
sCertificateARN :: Lens' SpekeKeyProvider (Maybe Text)
sCertificateARN = lens _sCertificateARN (\s a -> s {_sCertificateARN = a})

-- | Specify the URL to the key server that your SPEKE-compliant DRM key provider uses to provide keys for encrypting your content.
sURL :: Lens' SpekeKeyProvider (Maybe Text)
sURL = lens _sURL (\s a -> s {_sURL = a})

-- | Relates to SPEKE implementation. DRM system identifiers. DASH output groups support a max of two system ids. Other group types support one system id. See  https://dashif.org/identifiers/content_protection/ for more details.
sSystemIds :: Lens' SpekeKeyProvider [Text]
sSystemIds = lens _sSystemIds (\s a -> s {_sSystemIds = a}) . _Default . _Coerce

instance FromJSON SpekeKeyProvider where
  parseJSON =
    withObject
      "SpekeKeyProvider"
      ( \x ->
          SpekeKeyProvider'
            <$> (x .:? "resourceId")
            <*> (x .:? "certificateArn")
            <*> (x .:? "url")
            <*> (x .:? "systemIds" .!= mempty)
      )

instance Hashable SpekeKeyProvider

instance NFData SpekeKeyProvider

instance ToJSON SpekeKeyProvider where
  toJSON SpekeKeyProvider' {..} =
    object
      ( catMaybes
          [ ("resourceId" .=) <$> _sResourceId,
            ("certificateArn" .=) <$> _sCertificateARN,
            ("url" .=) <$> _sURL,
            ("systemIds" .=) <$> _sSystemIds
          ]
      )
