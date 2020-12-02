{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.IAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IAMInstanceProfile where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about the EC2 instance profile.
--
--
--
-- /See:/ 'iamInstanceProfile' smart constructor.
data IAMInstanceProfile = IAMInstanceProfile'
  { _iapARN ::
      !(Maybe Text),
    _iapId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapARN' - The profile ARN of the EC2 instance.
--
-- * 'iapId' - The profile ID of the EC2 instance.
iamInstanceProfile ::
  IAMInstanceProfile
iamInstanceProfile =
  IAMInstanceProfile' {_iapARN = Nothing, _iapId = Nothing}

-- | The profile ARN of the EC2 instance.
iapARN :: Lens' IAMInstanceProfile (Maybe Text)
iapARN = lens _iapARN (\s a -> s {_iapARN = a})

-- | The profile ID of the EC2 instance.
iapId :: Lens' IAMInstanceProfile (Maybe Text)
iapId = lens _iapId (\s a -> s {_iapId = a})

instance FromJSON IAMInstanceProfile where
  parseJSON =
    withObject
      "IAMInstanceProfile"
      (\x -> IAMInstanceProfile' <$> (x .:? "arn") <*> (x .:? "id"))

instance Hashable IAMInstanceProfile

instance NFData IAMInstanceProfile
