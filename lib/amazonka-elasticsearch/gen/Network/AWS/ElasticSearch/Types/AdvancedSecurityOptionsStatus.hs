{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AdvancedSecurityOptionsStatus where

import Network.AWS.ElasticSearch.Types.AdvancedSecurityOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the status of advanced security options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'advancedSecurityOptionsStatus' smart constructor.
data AdvancedSecurityOptionsStatus = AdvancedSecurityOptionsStatus'
  { _asosOptions ::
      !AdvancedSecurityOptions,
    _asosStatus :: !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdvancedSecurityOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asosOptions' - Specifies advanced security options for the specified Elasticsearch domain.
--
-- * 'asosStatus' - Status of the advanced security options for the specified Elasticsearch domain.
advancedSecurityOptionsStatus ::
  -- | 'asosOptions'
  AdvancedSecurityOptions ->
  -- | 'asosStatus'
  OptionStatus ->
  AdvancedSecurityOptionsStatus
advancedSecurityOptionsStatus pOptions_ pStatus_ =
  AdvancedSecurityOptionsStatus'
    { _asosOptions = pOptions_,
      _asosStatus = pStatus_
    }

-- | Specifies advanced security options for the specified Elasticsearch domain.
asosOptions :: Lens' AdvancedSecurityOptionsStatus AdvancedSecurityOptions
asosOptions = lens _asosOptions (\s a -> s {_asosOptions = a})

-- | Status of the advanced security options for the specified Elasticsearch domain.
asosStatus :: Lens' AdvancedSecurityOptionsStatus OptionStatus
asosStatus = lens _asosStatus (\s a -> s {_asosStatus = a})

instance FromJSON AdvancedSecurityOptionsStatus where
  parseJSON =
    withObject
      "AdvancedSecurityOptionsStatus"
      ( \x ->
          AdvancedSecurityOptionsStatus'
            <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable AdvancedSecurityOptionsStatus

instance NFData AdvancedSecurityOptionsStatus
