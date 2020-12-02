{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptionsStatus where

import Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the Encryption At Rest options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'encryptionAtRestOptionsStatus' smart constructor.
data EncryptionAtRestOptionsStatus = EncryptionAtRestOptionsStatus'
  { _earosOptions ::
      !EncryptionAtRestOptions,
    _earosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EncryptionAtRestOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'earosOptions' - Specifies the Encryption At Rest options for the specified Elasticsearch domain.
--
-- * 'earosStatus' - Specifies the status of the Encryption At Rest options for the specified Elasticsearch domain.
encryptionAtRestOptionsStatus ::
  -- | 'earosOptions'
  EncryptionAtRestOptions ->
  -- | 'earosStatus'
  OptionStatus ->
  EncryptionAtRestOptionsStatus
encryptionAtRestOptionsStatus pOptions_ pStatus_ =
  EncryptionAtRestOptionsStatus'
    { _earosOptions = pOptions_,
      _earosStatus = pStatus_
    }

-- | Specifies the Encryption At Rest options for the specified Elasticsearch domain.
earosOptions :: Lens' EncryptionAtRestOptionsStatus EncryptionAtRestOptions
earosOptions = lens _earosOptions (\s a -> s {_earosOptions = a})

-- | Specifies the status of the Encryption At Rest options for the specified Elasticsearch domain.
earosStatus :: Lens' EncryptionAtRestOptionsStatus OptionStatus
earosStatus = lens _earosStatus (\s a -> s {_earosStatus = a})

instance FromJSON EncryptionAtRestOptionsStatus where
  parseJSON =
    withObject
      "EncryptionAtRestOptionsStatus"
      ( \x ->
          EncryptionAtRestOptionsStatus'
            <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable EncryptionAtRestOptionsStatus

instance NFData EncryptionAtRestOptionsStatus
