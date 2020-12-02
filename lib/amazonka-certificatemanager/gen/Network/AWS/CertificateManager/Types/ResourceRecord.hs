{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.ResourceRecord
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.ResourceRecord where

import Network.AWS.CertificateManager.Types.RecordType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains a DNS record value that you can use to can use to validate ownership or control of a domain. This is used by the 'DescribeCertificate' action.
--
--
--
-- /See:/ 'resourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { _rrName :: !Text,
    _rrType :: !RecordType,
    _rrValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrName' - The name of the DNS record to create in your domain. This is supplied by ACM.
--
-- * 'rrType' - The type of DNS record. Currently this can be @CNAME@ .
--
-- * 'rrValue' - The value of the CNAME record to add to your DNS database. This is supplied by ACM.
resourceRecord ::
  -- | 'rrName'
  Text ->
  -- | 'rrType'
  RecordType ->
  -- | 'rrValue'
  Text ->
  ResourceRecord
resourceRecord pName_ pType_ pValue_ =
  ResourceRecord'
    { _rrName = pName_,
      _rrType = pType_,
      _rrValue = pValue_
    }

-- | The name of the DNS record to create in your domain. This is supplied by ACM.
rrName :: Lens' ResourceRecord Text
rrName = lens _rrName (\s a -> s {_rrName = a})

-- | The type of DNS record. Currently this can be @CNAME@ .
rrType :: Lens' ResourceRecord RecordType
rrType = lens _rrType (\s a -> s {_rrType = a})

-- | The value of the CNAME record to add to your DNS database. This is supplied by ACM.
rrValue :: Lens' ResourceRecord Text
rrValue = lens _rrValue (\s a -> s {_rrValue = a})

instance FromJSON ResourceRecord where
  parseJSON =
    withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            <$> (x .: "Name") <*> (x .: "Type") <*> (x .: "Value")
      )

instance Hashable ResourceRecord

instance NFData ResourceRecord
