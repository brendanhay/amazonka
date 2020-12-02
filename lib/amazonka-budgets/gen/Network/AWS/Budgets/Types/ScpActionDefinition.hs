{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ScpActionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ScpActionDefinition where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The service control policies (SCP) action definition details.
--
--
--
-- /See:/ 'scpActionDefinition' smart constructor.
data ScpActionDefinition = ScpActionDefinition'
  { _sadPolicyId ::
      !Text,
    _sadTargetIds :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScpActionDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sadPolicyId' - The policy ID attached.
--
-- * 'sadTargetIds' - A list of target IDs.
scpActionDefinition ::
  -- | 'sadPolicyId'
  Text ->
  -- | 'sadTargetIds'
  NonEmpty Text ->
  ScpActionDefinition
scpActionDefinition pPolicyId_ pTargetIds_ =
  ScpActionDefinition'
    { _sadPolicyId = pPolicyId_,
      _sadTargetIds = _List1 # pTargetIds_
    }

-- | The policy ID attached.
sadPolicyId :: Lens' ScpActionDefinition Text
sadPolicyId = lens _sadPolicyId (\s a -> s {_sadPolicyId = a})

-- | A list of target IDs.
sadTargetIds :: Lens' ScpActionDefinition (NonEmpty Text)
sadTargetIds = lens _sadTargetIds (\s a -> s {_sadTargetIds = a}) . _List1

instance FromJSON ScpActionDefinition where
  parseJSON =
    withObject
      "ScpActionDefinition"
      ( \x ->
          ScpActionDefinition' <$> (x .: "PolicyId") <*> (x .: "TargetIds")
      )

instance Hashable ScpActionDefinition

instance NFData ScpActionDefinition

instance ToJSON ScpActionDefinition where
  toJSON ScpActionDefinition' {..} =
    object
      ( catMaybes
          [ Just ("PolicyId" .= _sadPolicyId),
            Just ("TargetIds" .= _sadTargetIds)
          ]
      )
