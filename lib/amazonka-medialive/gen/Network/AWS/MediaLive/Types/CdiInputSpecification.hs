{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.CdiInputSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.CdiInputSpecification where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types.CdiInputResolution
import Network.AWS.Prelude

-- | Placeholder documentation for CdiInputSpecification
--
-- /See:/ 'cdiInputSpecification' smart constructor.
newtype CdiInputSpecification = CdiInputSpecification'
  { _cisResolution ::
      Maybe CdiInputResolution
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CdiInputSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cisResolution' - Maximum CDI input resolution
cdiInputSpecification ::
  CdiInputSpecification
cdiInputSpecification =
  CdiInputSpecification' {_cisResolution = Nothing}

-- | Maximum CDI input resolution
cisResolution :: Lens' CdiInputSpecification (Maybe CdiInputResolution)
cisResolution = lens _cisResolution (\s a -> s {_cisResolution = a})

instance FromJSON CdiInputSpecification where
  parseJSON =
    withObject
      "CdiInputSpecification"
      (\x -> CdiInputSpecification' <$> (x .:? "resolution"))

instance Hashable CdiInputSpecification

instance NFData CdiInputSpecification

instance ToJSON CdiInputSpecification where
  toJSON CdiInputSpecification' {..} =
    object (catMaybes [("resolution" .=) <$> _cisResolution])
