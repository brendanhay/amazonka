{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigningCertificateChain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigningCertificateChain where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the certificate chain being used when code signing a file.
--
--
--
-- /See:/ 'codeSigningCertificateChain' smart constructor.
data CodeSigningCertificateChain = CodeSigningCertificateChain'
  { _csccCertificateName ::
      !(Maybe Text),
    _csccInlineDocument ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeSigningCertificateChain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csccCertificateName' - The name of the certificate.
--
-- * 'csccInlineDocument' - A base64 encoded binary representation of the code signing certificate chain.
codeSigningCertificateChain ::
  CodeSigningCertificateChain
codeSigningCertificateChain =
  CodeSigningCertificateChain'
    { _csccCertificateName = Nothing,
      _csccInlineDocument = Nothing
    }

-- | The name of the certificate.
csccCertificateName :: Lens' CodeSigningCertificateChain (Maybe Text)
csccCertificateName = lens _csccCertificateName (\s a -> s {_csccCertificateName = a})

-- | A base64 encoded binary representation of the code signing certificate chain.
csccInlineDocument :: Lens' CodeSigningCertificateChain (Maybe Text)
csccInlineDocument = lens _csccInlineDocument (\s a -> s {_csccInlineDocument = a})

instance FromJSON CodeSigningCertificateChain where
  parseJSON =
    withObject
      "CodeSigningCertificateChain"
      ( \x ->
          CodeSigningCertificateChain'
            <$> (x .:? "certificateName") <*> (x .:? "inlineDocument")
      )

instance Hashable CodeSigningCertificateChain

instance NFData CodeSigningCertificateChain

instance ToJSON CodeSigningCertificateChain where
  toJSON CodeSigningCertificateChain' {..} =
    object
      ( catMaybes
          [ ("certificateName" .=) <$> _csccCertificateName,
            ("inlineDocument" .=) <$> _csccInlineDocument
          ]
      )
