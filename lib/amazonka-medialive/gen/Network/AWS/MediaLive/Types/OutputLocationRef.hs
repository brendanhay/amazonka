{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.OutputLocationRef
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.OutputLocationRef where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Reference to an OutputDestination ID defined in the channel
--
-- /See:/ 'outputLocationRef' smart constructor.
newtype OutputLocationRef = OutputLocationRef'
  { _olrDestinationRefId ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputLocationRef' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'olrDestinationRefId' - Undocumented member.
outputLocationRef ::
  OutputLocationRef
outputLocationRef =
  OutputLocationRef' {_olrDestinationRefId = Nothing}

-- | Undocumented member.
olrDestinationRefId :: Lens' OutputLocationRef (Maybe Text)
olrDestinationRefId = lens _olrDestinationRefId (\s a -> s {_olrDestinationRefId = a})

instance FromJSON OutputLocationRef where
  parseJSON =
    withObject
      "OutputLocationRef"
      (\x -> OutputLocationRef' <$> (x .:? "destinationRefId"))

instance Hashable OutputLocationRef

instance NFData OutputLocationRef

instance ToJSON OutputLocationRef where
  toJSON OutputLocationRef' {..} =
    object
      (catMaybes [("destinationRefId" .=) <$> _olrDestinationRefId])
