{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Creates settings for the require check in feature that are applied to a room profile. Require check in allows a meeting room’s Alexa or AVS device to prompt the user to check in; otherwise, the room will be released.
--
--
--
-- /See:/ 'createRequireCheckIn' smart constructor.
data CreateRequireCheckIn = CreateRequireCheckIn'
  { _crciReleaseAfterMinutes ::
      !Int,
    _crciEnabled :: !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRequireCheckIn' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crciReleaseAfterMinutes' - Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
--
-- * 'crciEnabled' - Whether require check in is enabled or not.
createRequireCheckIn ::
  -- | 'crciReleaseAfterMinutes'
  Int ->
  -- | 'crciEnabled'
  Bool ->
  CreateRequireCheckIn
createRequireCheckIn pReleaseAfterMinutes_ pEnabled_ =
  CreateRequireCheckIn'
    { _crciReleaseAfterMinutes =
        pReleaseAfterMinutes_,
      _crciEnabled = pEnabled_
    }

-- | Duration between 5 and 20 minutes to determine when to release the room if it's not checked into.
crciReleaseAfterMinutes :: Lens' CreateRequireCheckIn Int
crciReleaseAfterMinutes = lens _crciReleaseAfterMinutes (\s a -> s {_crciReleaseAfterMinutes = a})

-- | Whether require check in is enabled or not.
crciEnabled :: Lens' CreateRequireCheckIn Bool
crciEnabled = lens _crciEnabled (\s a -> s {_crciEnabled = a})

instance Hashable CreateRequireCheckIn

instance NFData CreateRequireCheckIn

instance ToJSON CreateRequireCheckIn where
  toJSON CreateRequireCheckIn' {..} =
    object
      ( catMaybes
          [ Just ("ReleaseAfterMinutes" .= _crciReleaseAfterMinutes),
            Just ("Enabled" .= _crciEnabled)
          ]
      )
