{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossFailoverSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossFailoverSettings where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | MediaLive will perform a failover if content is not detected in this input for the specified period.
--
-- /See:/ 'inputLossFailoverSettings' smart constructor.
newtype InputLossFailoverSettings = InputLossFailoverSettings'
  { _ilfsInputLossThresholdMsec ::
      Maybe Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLossFailoverSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilfsInputLossThresholdMsec' - The amount of time (in milliseconds) that no input is detected. After that time, an input failover will occur.
inputLossFailoverSettings ::
  InputLossFailoverSettings
inputLossFailoverSettings =
  InputLossFailoverSettings' {_ilfsInputLossThresholdMsec = Nothing}

-- | The amount of time (in milliseconds) that no input is detected. After that time, an input failover will occur.
ilfsInputLossThresholdMsec :: Lens' InputLossFailoverSettings (Maybe Natural)
ilfsInputLossThresholdMsec = lens _ilfsInputLossThresholdMsec (\s a -> s {_ilfsInputLossThresholdMsec = a}) . mapping _Nat

instance FromJSON InputLossFailoverSettings where
  parseJSON =
    withObject
      "InputLossFailoverSettings"
      ( \x ->
          InputLossFailoverSettings' <$> (x .:? "inputLossThresholdMsec")
      )

instance Hashable InputLossFailoverSettings

instance NFData InputLossFailoverSettings

instance ToJSON InputLossFailoverSettings where
  toJSON InputLossFailoverSettings' {..} =
    object
      ( catMaybes
          [("inputLossThresholdMsec" .=) <$> _ilfsInputLossThresholdMsec]
      )
