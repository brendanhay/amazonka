{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ResourceChangeDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ResourceChangeDetail where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ServiceCatalog.Types.EvaluationType
import Network.AWS.ServiceCatalog.Types.ResourceTargetDefinition

-- | Information about a change to a resource attribute.
--
--
--
-- /See:/ 'resourceChangeDetail' smart constructor.
data ResourceChangeDetail = ResourceChangeDetail'
  { _rcdCausingEntity ::
      !(Maybe Text),
    _rcdEvaluation :: !(Maybe EvaluationType),
    _rcdTarget :: !(Maybe ResourceTargetDefinition)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceChangeDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcdCausingEntity' - The ID of the entity that caused the change.
--
-- * 'rcdEvaluation' - For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
--
-- * 'rcdTarget' - Information about the resource attribute to be modified.
resourceChangeDetail ::
  ResourceChangeDetail
resourceChangeDetail =
  ResourceChangeDetail'
    { _rcdCausingEntity = Nothing,
      _rcdEvaluation = Nothing,
      _rcdTarget = Nothing
    }

-- | The ID of the entity that caused the change.
rcdCausingEntity :: Lens' ResourceChangeDetail (Maybe Text)
rcdCausingEntity = lens _rcdCausingEntity (\s a -> s {_rcdCausingEntity = a})

-- | For static evaluations, the value of the resource attribute will change and the new value is known. For dynamic evaluations, the value might change, and any new value will be determined when the plan is updated.
rcdEvaluation :: Lens' ResourceChangeDetail (Maybe EvaluationType)
rcdEvaluation = lens _rcdEvaluation (\s a -> s {_rcdEvaluation = a})

-- | Information about the resource attribute to be modified.
rcdTarget :: Lens' ResourceChangeDetail (Maybe ResourceTargetDefinition)
rcdTarget = lens _rcdTarget (\s a -> s {_rcdTarget = a})

instance FromJSON ResourceChangeDetail where
  parseJSON =
    withObject
      "ResourceChangeDetail"
      ( \x ->
          ResourceChangeDetail'
            <$> (x .:? "CausingEntity")
            <*> (x .:? "Evaluation")
            <*> (x .:? "Target")
      )

instance Hashable ResourceChangeDetail

instance NFData ResourceChangeDetail
