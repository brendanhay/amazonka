{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.OnFailure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnFailure where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A destination for events that failed processing.
--
--
--
-- /See:/ 'onFailure' smart constructor.
newtype OnFailure = OnFailure' {_ofDestination :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OnFailure' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ofDestination' - The Amazon Resource Name (ARN) of the destination resource.
onFailure ::
  OnFailure
onFailure = OnFailure' {_ofDestination = Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
ofDestination :: Lens' OnFailure (Maybe Text)
ofDestination = lens _ofDestination (\s a -> s {_ofDestination = a})

instance FromJSON OnFailure where
  parseJSON =
    withObject
      "OnFailure"
      (\x -> OnFailure' <$> (x .:? "Destination"))

instance Hashable OnFailure

instance NFData OnFailure

instance ToJSON OnFailure where
  toJSON OnFailure' {..} =
    object (catMaybes [("Destination" .=) <$> _ofDestination])
