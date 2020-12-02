{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.Types.KeyUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.KeyUsage where

import Network.AWS.CertificateManager.Types.KeyUsageName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Key Usage X.509 v3 extension defines the purpose of the public key contained in the certificate.
--
--
--
-- /See:/ 'keyUsage' smart constructor.
newtype KeyUsage = KeyUsage' {_kuName :: Maybe KeyUsageName}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kuName' - A string value that contains a Key Usage extension name.
keyUsage ::
  KeyUsage
keyUsage = KeyUsage' {_kuName = Nothing}

-- | A string value that contains a Key Usage extension name.
kuName :: Lens' KeyUsage (Maybe KeyUsageName)
kuName = lens _kuName (\s a -> s {_kuName = a})

instance FromJSON KeyUsage where
  parseJSON =
    withObject "KeyUsage" (\x -> KeyUsage' <$> (x .:? "Name"))

instance Hashable KeyUsage

instance NFData KeyUsage
