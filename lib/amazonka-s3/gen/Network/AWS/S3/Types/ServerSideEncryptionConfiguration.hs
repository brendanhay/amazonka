{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ServerSideEncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ServerSideEncryptionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ServerSideEncryptionRule

-- | Specifies the default server-side-encryption configuration.
--
--
--
-- /See:/ 'serverSideEncryptionConfiguration' smart constructor.
newtype ServerSideEncryptionConfiguration = ServerSideEncryptionConfiguration'
  { _ssecRules ::
      [ServerSideEncryptionRule]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServerSideEncryptionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssecRules' - Container for information about a particular server-side encryption configuration rule.
serverSideEncryptionConfiguration ::
  ServerSideEncryptionConfiguration
serverSideEncryptionConfiguration =
  ServerSideEncryptionConfiguration' {_ssecRules = mempty}

-- | Container for information about a particular server-side encryption configuration rule.
ssecRules :: Lens' ServerSideEncryptionConfiguration [ServerSideEncryptionRule]
ssecRules = lens _ssecRules (\s a -> s {_ssecRules = a}) . _Coerce

instance FromXML ServerSideEncryptionConfiguration where
  parseXML x =
    ServerSideEncryptionConfiguration' <$> (parseXMLList "Rule" x)

instance Hashable ServerSideEncryptionConfiguration

instance NFData ServerSideEncryptionConfiguration

instance ToXML ServerSideEncryptionConfiguration where
  toXML ServerSideEncryptionConfiguration' {..} =
    mconcat [toXMLList "Rule" _ssecRules]
