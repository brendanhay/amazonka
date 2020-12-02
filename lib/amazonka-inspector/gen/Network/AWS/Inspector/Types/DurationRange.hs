{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.DurationRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.DurationRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used in the 'AssessmentTemplateFilter' data type.
--
--
--
-- /See:/ 'durationRange' smart constructor.
data DurationRange = DurationRange'
  { _drMinSeconds :: !(Maybe Nat),
    _drMaxSeconds :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DurationRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drMinSeconds' - The minimum value of the duration range. Must be greater than zero.
--
-- * 'drMaxSeconds' - The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
durationRange ::
  DurationRange
durationRange =
  DurationRange' {_drMinSeconds = Nothing, _drMaxSeconds = Nothing}

-- | The minimum value of the duration range. Must be greater than zero.
drMinSeconds :: Lens' DurationRange (Maybe Natural)
drMinSeconds = lens _drMinSeconds (\s a -> s {_drMinSeconds = a}) . mapping _Nat

-- | The maximum value of the duration range. Must be less than or equal to 604800 seconds (1 week).
drMaxSeconds :: Lens' DurationRange (Maybe Natural)
drMaxSeconds = lens _drMaxSeconds (\s a -> s {_drMaxSeconds = a}) . mapping _Nat

instance Hashable DurationRange

instance NFData DurationRange

instance ToJSON DurationRange where
  toJSON DurationRange' {..} =
    object
      ( catMaybes
          [ ("minSeconds" .=) <$> _drMinSeconds,
            ("maxSeconds" .=) <$> _drMaxSeconds
          ]
      )
