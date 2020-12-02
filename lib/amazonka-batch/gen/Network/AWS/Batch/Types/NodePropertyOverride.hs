{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodePropertyOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodePropertyOverride where

import Network.AWS.Batch.Types.ContainerOverrides
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Object representing any node overrides to a job definition that is used in a 'SubmitJob' API operation.
--
--
--
-- /See:/ 'nodePropertyOverride' smart constructor.
data NodePropertyOverride = NodePropertyOverride'
  { _npoContainerOverrides ::
      !(Maybe ContainerOverrides),
    _npoTargetNodes :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodePropertyOverride' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npoContainerOverrides' - The overrides that should be sent to a node range.
--
-- * 'npoTargetNodes' - The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
nodePropertyOverride ::
  -- | 'npoTargetNodes'
  Text ->
  NodePropertyOverride
nodePropertyOverride pTargetNodes_ =
  NodePropertyOverride'
    { _npoContainerOverrides = Nothing,
      _npoTargetNodes = pTargetNodes_
    }

-- | The overrides that should be sent to a node range.
npoContainerOverrides :: Lens' NodePropertyOverride (Maybe ContainerOverrides)
npoContainerOverrides = lens _npoContainerOverrides (\s a -> s {_npoContainerOverrides = a})

-- | The range of nodes, using node index values, with which to override. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range.
npoTargetNodes :: Lens' NodePropertyOverride Text
npoTargetNodes = lens _npoTargetNodes (\s a -> s {_npoTargetNodes = a})

instance Hashable NodePropertyOverride

instance NFData NodePropertyOverride

instance ToJSON NodePropertyOverride where
  toJSON NodePropertyOverride' {..} =
    object
      ( catMaybes
          [ ("containerOverrides" .=) <$> _npoContainerOverrides,
            Just ("targetNodes" .= _npoTargetNodes)
          ]
      )
