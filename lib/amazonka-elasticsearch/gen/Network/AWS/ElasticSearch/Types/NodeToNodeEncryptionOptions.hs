{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.NodeToNodeEncryptionOptions where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the node-to-node encryption options.
--
--
--
-- /See:/ 'nodeToNodeEncryptionOptions' smart constructor.
newtype NodeToNodeEncryptionOptions = NodeToNodeEncryptionOptions'
  { _ntneoEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeToNodeEncryptionOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ntneoEnabled' - Specify true to enable node-to-node encryption.
nodeToNodeEncryptionOptions ::
  NodeToNodeEncryptionOptions
nodeToNodeEncryptionOptions =
  NodeToNodeEncryptionOptions' {_ntneoEnabled = Nothing}

-- | Specify true to enable node-to-node encryption.
ntneoEnabled :: Lens' NodeToNodeEncryptionOptions (Maybe Bool)
ntneoEnabled = lens _ntneoEnabled (\s a -> s {_ntneoEnabled = a})

instance FromJSON NodeToNodeEncryptionOptions where
  parseJSON =
    withObject
      "NodeToNodeEncryptionOptions"
      (\x -> NodeToNodeEncryptionOptions' <$> (x .:? "Enabled"))

instance Hashable NodeToNodeEncryptionOptions

instance NFData NodeToNodeEncryptionOptions

instance ToJSON NodeToNodeEncryptionOptions where
  toJSON NodeToNodeEncryptionOptions' {..} =
    object (catMaybes [("Enabled" .=) <$> _ntneoEnabled])
