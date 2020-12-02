{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Volume where

import Network.AWS.Batch.Types.Host
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A data volume used in a job's container properties.
--
--
--
-- /See:/ 'volume' smart constructor.
data Volume = Volume'
  { _vName :: !(Maybe Text),
    _vHost :: !(Maybe Host)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vName' - The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
--
-- * 'vHost' - The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
volume ::
  Volume
volume = Volume' {_vName = Nothing, _vHost = Nothing}

-- | The name of the volume. Up to 255 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed. This name is referenced in the @sourceVolume@ parameter of container definition @mountPoints@ .
vName :: Lens' Volume (Maybe Text)
vName = lens _vName (\s a -> s {_vName = a})

-- | The contents of the @host@ parameter determine whether your data volume persists on the host container instance and where it is stored. If the host parameter is empty, then the Docker daemon assigns a host path for your data volume. However, the data is not guaranteed to persist after the containers associated with it stop running.
vHost :: Lens' Volume (Maybe Host)
vHost = lens _vHost (\s a -> s {_vHost = a})

instance FromJSON Volume where
  parseJSON =
    withObject
      "Volume"
      (\x -> Volume' <$> (x .:? "name") <*> (x .:? "host"))

instance Hashable Volume

instance NFData Volume

instance ToJSON Volume where
  toJSON Volume' {..} =
    object
      (catMaybes [("name" .=) <$> _vName, ("host" .=) <$> _vHost])
