{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.MapStateStartedEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapStateStartedEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details about a Map state that was started.
--
--
--
-- /See:/ 'mapStateStartedEventDetails' smart constructor.
newtype MapStateStartedEventDetails = MapStateStartedEventDetails'
  { _mssedLength ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MapStateStartedEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mssedLength' - The size of the array for Map state iterations.
mapStateStartedEventDetails ::
  MapStateStartedEventDetails
mapStateStartedEventDetails =
  MapStateStartedEventDetails' {_mssedLength = Nothing}

-- | The size of the array for Map state iterations.
mssedLength :: Lens' MapStateStartedEventDetails (Maybe Natural)
mssedLength = lens _mssedLength (\s a -> s {_mssedLength = a}) . mapping _Nat

instance FromJSON MapStateStartedEventDetails where
  parseJSON =
    withObject
      "MapStateStartedEventDetails"
      (\x -> MapStateStartedEventDetails' <$> (x .:? "length"))

instance Hashable MapStateStartedEventDetails

instance NFData MapStateStartedEventDetails
