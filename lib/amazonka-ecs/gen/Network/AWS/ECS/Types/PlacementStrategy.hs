{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.PlacementStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.PlacementStrategy where

import Network.AWS.ECS.Types.PlacementStrategyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The task placement strategy for a task or service. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-placement-strategies.html Task Placement Strategies> in the /Amazon Elastic Container Service Developer Guide/ .
--
--
--
-- /See:/ 'placementStrategy' smart constructor.
data PlacementStrategy = PlacementStrategy'
  { _psField ::
      !(Maybe Text),
    _psType :: !(Maybe PlacementStrategyType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PlacementStrategy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psField' - The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
--
-- * 'psType' - The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
placementStrategy ::
  PlacementStrategy
placementStrategy =
  PlacementStrategy' {_psField = Nothing, _psType = Nothing}

-- | The field to apply the placement strategy against. For the @spread@ placement strategy, valid values are @instanceId@ (or @host@ , which has the same effect), or any platform or custom attribute that is applied to a container instance, such as @attribute:ecs.availability-zone@ . For the @binpack@ placement strategy, valid values are @cpu@ and @memory@ . For the @random@ placement strategy, this field is not used.
psField :: Lens' PlacementStrategy (Maybe Text)
psField = lens _psField (\s a -> s {_psField = a})

-- | The type of placement strategy. The @random@ placement strategy randomly places tasks on available candidates. The @spread@ placement strategy spreads placement across available candidates evenly based on the @field@ parameter. The @binpack@ strategy places tasks on available candidates that have the least available amount of the resource that is specified with the @field@ parameter. For example, if you binpack on memory, a task is placed on the instance with the least amount of remaining memory (but still enough to run the task).
psType :: Lens' PlacementStrategy (Maybe PlacementStrategyType)
psType = lens _psType (\s a -> s {_psType = a})

instance FromJSON PlacementStrategy where
  parseJSON =
    withObject
      "PlacementStrategy"
      (\x -> PlacementStrategy' <$> (x .:? "field") <*> (x .:? "type"))

instance Hashable PlacementStrategy

instance NFData PlacementStrategy

instance ToJSON PlacementStrategy where
  toJSON PlacementStrategy' {..} =
    object
      (catMaybes [("field" .=) <$> _psField, ("type" .=) <$> _psType])
