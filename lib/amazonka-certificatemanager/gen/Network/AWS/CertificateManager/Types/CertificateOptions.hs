{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.CertificateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateOptions where

import Network.AWS.CertificateManager.Types.CertificateTransparencyLoggingPreference
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Structure that contains options for your certificate. Currently, you can use this only to specify whether to opt in to or out of certificate transparency logging. Some browsers require that public certificates issued for your domain be recorded in a log. Certificates that are not logged typically generate a browser error. Transparency makes it possible for you to detect SSL/TLS certificates that have been mistakenly or maliciously issued for your domain. For general information, see <https://docs.aws.amazon.com/acm/latest/userguide/acm-concepts.html#concept-transparency Certificate Transparency Logging> .
--
--
--
-- /See:/ 'certificateOptions' smart constructor.
newtype CertificateOptions = CertificateOptions'
  { _coCertificateTransparencyLoggingPreference ::
      Maybe CertificateTransparencyLoggingPreference
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCertificateTransparencyLoggingPreference' - You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ .
certificateOptions ::
  CertificateOptions
certificateOptions =
  CertificateOptions'
    { _coCertificateTransparencyLoggingPreference =
        Nothing
    }

-- | You can opt out of certificate transparency logging by specifying the @DISABLED@ option. Opt in by specifying @ENABLED@ .
coCertificateTransparencyLoggingPreference :: Lens' CertificateOptions (Maybe CertificateTransparencyLoggingPreference)
coCertificateTransparencyLoggingPreference = lens _coCertificateTransparencyLoggingPreference (\s a -> s {_coCertificateTransparencyLoggingPreference = a})

instance FromJSON CertificateOptions where
  parseJSON =
    withObject
      "CertificateOptions"
      ( \x ->
          CertificateOptions'
            <$> (x .:? "CertificateTransparencyLoggingPreference")
      )

instance Hashable CertificateOptions

instance NFData CertificateOptions

instance ToJSON CertificateOptions where
  toJSON CertificateOptions' {..} =
    object
      ( catMaybes
          [ ("CertificateTransparencyLoggingPreference" .=)
              <$> _coCertificateTransparencyLoggingPreference
          ]
      )
