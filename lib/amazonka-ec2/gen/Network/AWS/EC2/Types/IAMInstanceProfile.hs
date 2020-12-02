{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfile where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IAM instance profile.
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
-- * 'iapARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'iapId' - The ID of the instance profile.
iamInstanceProfile ::
  IAMInstanceProfile
iamInstanceProfile =
  IAMInstanceProfile' {_iapARN = Nothing, _iapId = Nothing}

-- | The Amazon Resource Name (ARN) of the instance profile.
iapARN :: Lens' IAMInstanceProfile (Maybe Text)
iapARN = lens _iapARN (\s a -> s {_iapARN = a})

-- | The ID of the instance profile.
iapId :: Lens' IAMInstanceProfile (Maybe Text)
iapId = lens _iapId (\s a -> s {_iapId = a})

instance FromXML IAMInstanceProfile where
  parseXML x = IAMInstanceProfile' <$> (x .@? "arn") <*> (x .@? "id")

instance Hashable IAMInstanceProfile

instance NFData IAMInstanceProfile
