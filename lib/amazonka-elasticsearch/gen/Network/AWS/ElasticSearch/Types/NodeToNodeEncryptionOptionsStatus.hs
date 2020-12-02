{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptionsStatus where

import Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
import Network.AWS.ElasticSearch.Types.OptionStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Status of the node-to-node encryption options for the specified Elasticsearch domain.
--
--
--
-- /See:/ 'nodeToNodeEncryptionOptionsStatus' smart constructor.
data NodeToNodeEncryptionOptionsStatus = NodeToNodeEncryptionOptionsStatus'
  { _ntneosOptions ::
      !NodeToNodeEncryptionOptions,
    _ntneosStatus ::
      !OptionStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeToNodeEncryptionOptionsStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ntneosOptions' - Specifies the node-to-node encryption options for the specified Elasticsearch domain.
--
-- * 'ntneosStatus' - Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
nodeToNodeEncryptionOptionsStatus ::
  -- | 'ntneosOptions'
  NodeToNodeEncryptionOptions ->
  -- | 'ntneosStatus'
  OptionStatus ->
  NodeToNodeEncryptionOptionsStatus
nodeToNodeEncryptionOptionsStatus pOptions_ pStatus_ =
  NodeToNodeEncryptionOptionsStatus'
    { _ntneosOptions = pOptions_,
      _ntneosStatus = pStatus_
    }

-- | Specifies the node-to-node encryption options for the specified Elasticsearch domain.
ntneosOptions :: Lens' NodeToNodeEncryptionOptionsStatus NodeToNodeEncryptionOptions
ntneosOptions = lens _ntneosOptions (\s a -> s {_ntneosOptions = a})

-- | Specifies the status of the node-to-node encryption options for the specified Elasticsearch domain.
ntneosStatus :: Lens' NodeToNodeEncryptionOptionsStatus OptionStatus
ntneosStatus = lens _ntneosStatus (\s a -> s {_ntneosStatus = a})

instance FromJSON NodeToNodeEncryptionOptionsStatus where
  parseJSON =
    withObject
      "NodeToNodeEncryptionOptionsStatus"
      ( \x ->
          NodeToNodeEncryptionOptionsStatus'
            <$> (x .: "Options") <*> (x .: "Status")
      )

instance Hashable NodeToNodeEncryptionOptionsStatus

instance NFData NodeToNodeEncryptionOptionsStatus
