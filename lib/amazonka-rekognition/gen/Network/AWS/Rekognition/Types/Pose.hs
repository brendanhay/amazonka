{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Pose
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Pose where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates the pose of the face as determined by its pitch, roll, and yaw.
--
--
--
-- /See:/ 'pose' smart constructor.
data Pose = Pose'
  { _pYaw :: !(Maybe Double),
    _pRoll :: !(Maybe Double),
    _pPitch :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Pose' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pYaw' - Value representing the face rotation on the yaw axis.
--
-- * 'pRoll' - Value representing the face rotation on the roll axis.
--
-- * 'pPitch' - Value representing the face rotation on the pitch axis.
pose ::
  Pose
pose = Pose' {_pYaw = Nothing, _pRoll = Nothing, _pPitch = Nothing}

-- | Value representing the face rotation on the yaw axis.
pYaw :: Lens' Pose (Maybe Double)
pYaw = lens _pYaw (\s a -> s {_pYaw = a})

-- | Value representing the face rotation on the roll axis.
pRoll :: Lens' Pose (Maybe Double)
pRoll = lens _pRoll (\s a -> s {_pRoll = a})

-- | Value representing the face rotation on the pitch axis.
pPitch :: Lens' Pose (Maybe Double)
pPitch = lens _pPitch (\s a -> s {_pPitch = a})

instance FromJSON Pose where
  parseJSON =
    withObject
      "Pose"
      ( \x ->
          Pose' <$> (x .:? "Yaw") <*> (x .:? "Roll") <*> (x .:? "Pitch")
      )

instance Hashable Pose

instance NFData Pose
