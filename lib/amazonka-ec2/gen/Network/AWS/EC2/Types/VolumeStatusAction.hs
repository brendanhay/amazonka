{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VolumeStatusAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeStatusAction where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a volume status operation code.
--
--
--
-- /See:/ 'volumeStatusAction' smart constructor.
data VolumeStatusAction = VolumeStatusAction'
  { _vsaEventType ::
      !(Maybe Text),
    _vsaCode :: !(Maybe Text),
    _vsaDescription :: !(Maybe Text),
    _vsaEventId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VolumeStatusAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vsaEventType' - The event type associated with this operation.
--
-- * 'vsaCode' - The code identifying the operation, for example, @enable-volume-io@ .
--
-- * 'vsaDescription' - A description of the operation.
--
-- * 'vsaEventId' - The ID of the event associated with this operation.
volumeStatusAction ::
  VolumeStatusAction
volumeStatusAction =
  VolumeStatusAction'
    { _vsaEventType = Nothing,
      _vsaCode = Nothing,
      _vsaDescription = Nothing,
      _vsaEventId = Nothing
    }

-- | The event type associated with this operation.
vsaEventType :: Lens' VolumeStatusAction (Maybe Text)
vsaEventType = lens _vsaEventType (\s a -> s {_vsaEventType = a})

-- | The code identifying the operation, for example, @enable-volume-io@ .
vsaCode :: Lens' VolumeStatusAction (Maybe Text)
vsaCode = lens _vsaCode (\s a -> s {_vsaCode = a})

-- | A description of the operation.
vsaDescription :: Lens' VolumeStatusAction (Maybe Text)
vsaDescription = lens _vsaDescription (\s a -> s {_vsaDescription = a})

-- | The ID of the event associated with this operation.
vsaEventId :: Lens' VolumeStatusAction (Maybe Text)
vsaEventId = lens _vsaEventId (\s a -> s {_vsaEventId = a})

instance FromXML VolumeStatusAction where
  parseXML x =
    VolumeStatusAction'
      <$> (x .@? "eventType")
      <*> (x .@? "code")
      <*> (x .@? "description")
      <*> (x .@? "eventId")

instance Hashable VolumeStatusAction

instance NFData VolumeStatusAction
