{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BlobAttributeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BlobAttributeValue where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'blobAttributeValue' smart constructor.
newtype BlobAttributeValue = BlobAttributeValue'
  { _bavValue ::
      Maybe Base64
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BlobAttributeValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bavValue' - Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
blobAttributeValue ::
  BlobAttributeValue
blobAttributeValue = BlobAttributeValue' {_bavValue = Nothing}

-- | Undocumented member.-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
bavValue :: Lens' BlobAttributeValue (Maybe ByteString)
bavValue = lens _bavValue (\s a -> s {_bavValue = a}) . mapping _Base64

instance Hashable BlobAttributeValue

instance NFData BlobAttributeValue

instance ToQuery BlobAttributeValue where
  toQuery BlobAttributeValue' {..} = mconcat ["Value" =: _bavValue]
