{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.MapIterationEventDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.MapIterationEventDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about an iteration of a Map state.
--
--
--
-- /See:/ 'mapIterationEventDetails' smart constructor.
data MapIterationEventDetails = MapIterationEventDetails'
  { _miedName ::
      !(Maybe Text),
    _miedIndex :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MapIterationEventDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'miedName' - The name of the iteration’s parent Map state.
--
-- * 'miedIndex' - The index of the array belonging to the Map state iteration.
mapIterationEventDetails ::
  MapIterationEventDetails
mapIterationEventDetails =
  MapIterationEventDetails'
    { _miedName = Nothing,
      _miedIndex = Nothing
    }

-- | The name of the iteration’s parent Map state.
miedName :: Lens' MapIterationEventDetails (Maybe Text)
miedName = lens _miedName (\s a -> s {_miedName = a})

-- | The index of the array belonging to the Map state iteration.
miedIndex :: Lens' MapIterationEventDetails (Maybe Natural)
miedIndex = lens _miedIndex (\s a -> s {_miedIndex = a}) . mapping _Nat

instance FromJSON MapIterationEventDetails where
  parseJSON =
    withObject
      "MapIterationEventDetails"
      ( \x ->
          MapIterationEventDetails' <$> (x .:? "name") <*> (x .:? "index")
      )

instance Hashable MapIterationEventDetails

instance NFData MapIterationEventDetails
