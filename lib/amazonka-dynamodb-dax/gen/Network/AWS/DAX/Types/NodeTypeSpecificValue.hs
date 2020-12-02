{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.NodeTypeSpecificValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DAX.Types.NodeTypeSpecificValue where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a parameter value that is applicable to a particular node type.
--
--
--
-- /See:/ 'nodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { _ntsvValue ::
      !(Maybe Text),
    _ntsvNodeType :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ntsvValue' - The parameter value for this node type.
--
-- * 'ntsvNodeType' - A node type to which the parameter value applies.
nodeTypeSpecificValue ::
  NodeTypeSpecificValue
nodeTypeSpecificValue =
  NodeTypeSpecificValue'
    { _ntsvValue = Nothing,
      _ntsvNodeType = Nothing
    }

-- | The parameter value for this node type.
ntsvValue :: Lens' NodeTypeSpecificValue (Maybe Text)
ntsvValue = lens _ntsvValue (\s a -> s {_ntsvValue = a})

-- | A node type to which the parameter value applies.
ntsvNodeType :: Lens' NodeTypeSpecificValue (Maybe Text)
ntsvNodeType = lens _ntsvNodeType (\s a -> s {_ntsvNodeType = a})

instance FromJSON NodeTypeSpecificValue where
  parseJSON =
    withObject
      "NodeTypeSpecificValue"
      ( \x ->
          NodeTypeSpecificValue' <$> (x .:? "Value") <*> (x .:? "NodeType")
      )

instance Hashable NodeTypeSpecificValue

instance NFData NodeTypeSpecificValue
