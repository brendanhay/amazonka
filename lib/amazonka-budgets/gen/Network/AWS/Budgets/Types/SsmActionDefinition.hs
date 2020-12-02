{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.SsmActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.SsmActionDefinition where

import Network.AWS.Budgets.Types.ActionSubType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The AWS Systems Manager (SSM) action definition details.
--
--
--
-- /See:/ 'ssmActionDefinition' smart constructor.
data SsmActionDefinition = SsmActionDefinition'
  { _sadActionSubType ::
      !ActionSubType,
    _sadRegion :: !Text,
    _sadInstanceIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SsmActionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sadActionSubType' - The action subType.
--
-- * 'sadRegion' - The Region to run the SSM document.
--
-- * 'sadInstanceIds' - The EC2 and RDS instance IDs.
ssmActionDefinition ::
  -- | 'sadActionSubType'
  ActionSubType ->
  -- | 'sadRegion'
  Text ->
  -- | 'sadInstanceIds'
  NonEmpty Text ->
  SsmActionDefinition
ssmActionDefinition pActionSubType_ pRegion_ pInstanceIds_ =
  SsmActionDefinition'
    { _sadActionSubType = pActionSubType_,
      _sadRegion = pRegion_,
      _sadInstanceIds = _List1 # pInstanceIds_
    }

-- | The action subType.
sadActionSubType :: Lens' SsmActionDefinition ActionSubType
sadActionSubType = lens _sadActionSubType (\s a -> s {_sadActionSubType = a})

-- | The Region to run the SSM document.
sadRegion :: Lens' SsmActionDefinition Text
sadRegion = lens _sadRegion (\s a -> s {_sadRegion = a})

-- | The EC2 and RDS instance IDs.
sadInstanceIds :: Lens' SsmActionDefinition (NonEmpty Text)
sadInstanceIds = lens _sadInstanceIds (\s a -> s {_sadInstanceIds = a}) . _List1

instance FromJSON SsmActionDefinition where
  parseJSON =
    withObject
      "SsmActionDefinition"
      ( \x ->
          SsmActionDefinition'
            <$> (x .: "ActionSubType")
            <*> (x .: "Region")
            <*> (x .: "InstanceIds")
      )

instance Hashable SsmActionDefinition

instance NFData SsmActionDefinition

instance ToJSON SsmActionDefinition where
  toJSON SsmActionDefinition' {..} =
    object
      ( catMaybes
          [ Just ("ActionSubType" .= _sadActionSubType),
            Just ("Region" .= _sadRegion),
            Just ("InstanceIds" .= _sadInstanceIds)
          ]
      )
