{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.TaskDefinitionPlacementConstraint where

import Network.AWS.ECS.Types.TaskDefinitionPlacementConstraintType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a constraint on task placement in the task definition. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-constraints.html Task Placement Constraints> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'taskDefinitionPlacementConstraint' smart constructor.
data TaskDefinitionPlacementConstraint = TaskDefinitionPlacementConstraint'
  { _tdpcExpression ::
      !(Maybe Text),
    _tdpcType ::
      !( Maybe
           TaskDefinitionPlacementConstraintType
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TaskDefinitionPlacementConstraint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tdpcExpression' - A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'tdpcType' - The type of constraint. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
taskDefinitionPlacementConstraint ::
  TaskDefinitionPlacementConstraint
taskDefinitionPlacementConstraint =
  TaskDefinitionPlacementConstraint'
    { _tdpcExpression = Nothing,
      _tdpcType = Nothing
    }

-- | A cluster query language expression to apply to the constraint. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/cluster-query-language.html Cluster Query Language> in the /Amazon Elastic Container Service Developer Guide/ .
tdpcExpression :: Lens' TaskDefinitionPlacementConstraint (Maybe Text)
tdpcExpression = lens _tdpcExpression (\s a -> s {_tdpcExpression = a})

-- | The type of constraint. The @MemberOf@ constraint restricts selection to be from a group of valid candidates.
tdpcType :: Lens' TaskDefinitionPlacementConstraint (Maybe TaskDefinitionPlacementConstraintType)
tdpcType = lens _tdpcType (\s a -> s {_tdpcType = a})

instance FromJSON TaskDefinitionPlacementConstraint where
  parseJSON =
    withObject
      "TaskDefinitionPlacementConstraint"
      ( \x ->
          TaskDefinitionPlacementConstraint'
            <$> (x .:? "expression") <*> (x .:? "type")
      )

instance Hashable TaskDefinitionPlacementConstraint

instance NFData TaskDefinitionPlacementConstraint

instance ToJSON TaskDefinitionPlacementConstraint where
  toJSON TaskDefinitionPlacementConstraint' {..} =
    object
      ( catMaybes
          [("expression" .=) <$> _tdpcExpression, ("type" .=) <$> _tdpcType]
      )
