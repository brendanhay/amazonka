{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigningSignature
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigningSignature where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the signature for a file.
--
--
--
-- /See:/ 'codeSigningSignature' smart constructor.
newtype CodeSigningSignature = CodeSigningSignature'
  { _cssInlineDocument ::
      Maybe Base64
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CodeSigningSignature' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cssInlineDocument' - A base64 encoded binary representation of the code signing signature.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
codeSigningSignature ::
  CodeSigningSignature
codeSigningSignature =
  CodeSigningSignature' {_cssInlineDocument = Nothing}

-- | A base64 encoded binary representation of the code signing signature.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
cssInlineDocument :: Lens' CodeSigningSignature (Maybe ByteString)
cssInlineDocument = lens _cssInlineDocument (\s a -> s {_cssInlineDocument = a}) . mapping _Base64

instance FromJSON CodeSigningSignature where
  parseJSON =
    withObject
      "CodeSigningSignature"
      (\x -> CodeSigningSignature' <$> (x .:? "inlineDocument"))

instance Hashable CodeSigningSignature

instance NFData CodeSigningSignature

instance ToJSON CodeSigningSignature where
  toJSON CodeSigningSignature' {..} =
    object (catMaybes [("inlineDocument" .=) <$> _cssInlineDocument])
