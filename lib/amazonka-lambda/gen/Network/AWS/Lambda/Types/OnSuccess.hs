{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.OnSuccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.OnSuccess where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A destination for events that were processed successfully.
--
--
--
-- /See:/ 'onSuccess' smart constructor.
newtype OnSuccess = OnSuccess' {_osDestination :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OnSuccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osDestination' - The Amazon Resource Name (ARN) of the destination resource.
onSuccess ::
  OnSuccess
onSuccess = OnSuccess' {_osDestination = Nothing}

-- | The Amazon Resource Name (ARN) of the destination resource.
osDestination :: Lens' OnSuccess (Maybe Text)
osDestination = lens _osDestination (\s a -> s {_osDestination = a})

instance FromJSON OnSuccess where
  parseJSON =
    withObject
      "OnSuccess"
      (\x -> OnSuccess' <$> (x .:? "Destination"))

instance Hashable OnSuccess

instance NFData OnSuccess

instance ToJSON OnSuccess where
  toJSON OnSuccess' {..} =
    object (catMaybes [("Destination" .=) <$> _osDestination])
