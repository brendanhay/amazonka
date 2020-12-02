{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.OrganizationNode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.OrganizationNode where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.OrganizationNodeType

-- | Information about the organization node.
--
--
--
-- /See:/ 'organizationNode' smart constructor.
data OrganizationNode = OrganizationNode'
  { _onValue ::
      !(Maybe Text),
    _onType :: !(Maybe OrganizationNodeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'onValue' - The identifier of the organization node.
--
-- * 'onType' - The organization node type.
organizationNode ::
  OrganizationNode
organizationNode =
  OrganizationNode' {_onValue = Nothing, _onType = Nothing}

-- | The identifier of the organization node.
onValue :: Lens' OrganizationNode (Maybe Text)
onValue = lens _onValue (\s a -> s {_onValue = a})

-- | The organization node type.
onType :: Lens' OrganizationNode (Maybe OrganizationNodeType)
onType = lens _onType (\s a -> s {_onType = a})

instance FromJSON OrganizationNode where
  parseJSON =
    withObject
      "OrganizationNode"
      (\x -> OrganizationNode' <$> (x .:? "Value") <*> (x .:? "Type"))

instance Hashable OrganizationNode

instance NFData OrganizationNode

instance ToJSON OrganizationNode where
  toJSON OrganizationNode' {..} =
    object
      (catMaybes [("Value" .=) <$> _onValue, ("Type" .=) <$> _onType])
