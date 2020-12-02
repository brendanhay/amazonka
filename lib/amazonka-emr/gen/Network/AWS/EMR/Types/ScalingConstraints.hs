{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScalingConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingConstraints where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The upper and lower EC2 instance limits for an automatic scaling policy. Automatic scaling activities triggered by automatic scaling rules will not cause an instance group to grow above or below these limits.
--
--
--
-- /See:/ 'scalingConstraints' smart constructor.
data ScalingConstraints = ScalingConstraints'
  { _scMinCapacity ::
      !Int,
    _scMaxCapacity :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScalingConstraints' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scMinCapacity' - The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
--
-- * 'scMaxCapacity' - The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
scalingConstraints ::
  -- | 'scMinCapacity'
  Int ->
  -- | 'scMaxCapacity'
  Int ->
  ScalingConstraints
scalingConstraints pMinCapacity_ pMaxCapacity_ =
  ScalingConstraints'
    { _scMinCapacity = pMinCapacity_,
      _scMaxCapacity = pMaxCapacity_
    }

-- | The lower boundary of EC2 instances in an instance group below which scaling activities are not allowed to shrink. Scale-in activities will not terminate instances below this boundary.
scMinCapacity :: Lens' ScalingConstraints Int
scMinCapacity = lens _scMinCapacity (\s a -> s {_scMinCapacity = a})

-- | The upper boundary of EC2 instances in an instance group beyond which scaling activities are not allowed to grow. Scale-out activities will not add instances beyond this boundary.
scMaxCapacity :: Lens' ScalingConstraints Int
scMaxCapacity = lens _scMaxCapacity (\s a -> s {_scMaxCapacity = a})

instance FromJSON ScalingConstraints where
  parseJSON =
    withObject
      "ScalingConstraints"
      ( \x ->
          ScalingConstraints'
            <$> (x .: "MinCapacity") <*> (x .: "MaxCapacity")
      )

instance Hashable ScalingConstraints

instance NFData ScalingConstraints

instance ToJSON ScalingConstraints where
  toJSON ScalingConstraints' {..} =
    object
      ( catMaybes
          [ Just ("MinCapacity" .= _scMinCapacity),
            Just ("MaxCapacity" .= _scMaxCapacity)
          ]
      )
