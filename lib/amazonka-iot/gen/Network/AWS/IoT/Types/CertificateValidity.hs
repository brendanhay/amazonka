{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateValidity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateValidity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When the certificate is valid.
--
--
--
-- /See:/ 'certificateValidity' smart constructor.
data CertificateValidity = CertificateValidity'
  { _cvNotBefore ::
      !(Maybe POSIX),
    _cvNotAfter :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CertificateValidity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvNotBefore' - The certificate is not valid before this date.
--
-- * 'cvNotAfter' - The certificate is not valid after this date.
certificateValidity ::
  CertificateValidity
certificateValidity =
  CertificateValidity'
    { _cvNotBefore = Nothing,
      _cvNotAfter = Nothing
    }

-- | The certificate is not valid before this date.
cvNotBefore :: Lens' CertificateValidity (Maybe UTCTime)
cvNotBefore = lens _cvNotBefore (\s a -> s {_cvNotBefore = a}) . mapping _Time

-- | The certificate is not valid after this date.
cvNotAfter :: Lens' CertificateValidity (Maybe UTCTime)
cvNotAfter = lens _cvNotAfter (\s a -> s {_cvNotAfter = a}) . mapping _Time

instance FromJSON CertificateValidity where
  parseJSON =
    withObject
      "CertificateValidity"
      ( \x ->
          CertificateValidity'
            <$> (x .:? "notBefore") <*> (x .:? "notAfter")
      )

instance Hashable CertificateValidity

instance NFData CertificateValidity
