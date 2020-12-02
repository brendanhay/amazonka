{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementConstraint where

import Network.AWS.ECS.Types.PlacementConstraintType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a constraint on task placement. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'placementConstraint' smart constructor.
data PlacementConstraint = PlacementConstraint'
  { _pcExpression ::
      !(Maybe Text),
    _pcType :: !(Maybe PlacementConstraintType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacementConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcExpression' - A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'pcType' - The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
placementConstraint ::
  PlacementConstraint
placementConstraint =
  PlacementConstraint' {_pcExpression = Nothing, _pcType = Nothing}

-- | A cluster query language expression to apply to the constraint. You cannot specify an expression if the constraint type is @distinctInstance@ . For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
pcExpression :: Lens' PlacementConstraint (Maybe Text)
pcExpression = lens _pcExpression (\s a -> s {_pcExpression = a})

-- | The type of constraint. Use @distinctInstance@ to ensure that each task in a particular group is running on a different container instance. Use @memberOf@ to restrict the selection to a group of valid candidates.
pcType :: Lens' PlacementConstraint (Maybe PlacementConstraintType)
pcType = lens _pcType (\s a -> s {_pcType = a})

instance FromJSON PlacementConstraint where
  parseJSON =
    withObject
      "PlacementConstraint"
      ( \x ->
          PlacementConstraint' <$> (x .:? "expression") <*> (x .:? "type")
      )

instance Hashable PlacementConstraint

instance NFData PlacementConstraint

instance ToJSON PlacementConstraint where
  toJSON PlacementConstraint' {..} =
    object
      ( catMaybes
          [("expression" .=) <$> _pcExpression, ("type" .=) <$> _pcType]
      )
