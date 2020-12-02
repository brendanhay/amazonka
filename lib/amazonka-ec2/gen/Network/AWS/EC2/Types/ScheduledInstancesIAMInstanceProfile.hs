{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesIAMInstanceProfile where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IAM instance profile for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstancesIAMInstanceProfile' smart constructor.
data ScheduledInstancesIAMInstanceProfile = ScheduledInstancesIAMInstanceProfile'
  { _siiapARN ::
      !(Maybe Text),
    _siiapName ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstancesIAMInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'siiapARN' - The Amazon Resource Name (ARN).
--
-- * 'siiapName' - The name.
scheduledInstancesIAMInstanceProfile ::
  ScheduledInstancesIAMInstanceProfile
scheduledInstancesIAMInstanceProfile =
  ScheduledInstancesIAMInstanceProfile'
    { _siiapARN = Nothing,
      _siiapName = Nothing
    }

-- | The Amazon Resource Name (ARN).
siiapARN :: Lens' ScheduledInstancesIAMInstanceProfile (Maybe Text)
siiapARN = lens _siiapARN (\s a -> s {_siiapARN = a})

-- | The name.
siiapName :: Lens' ScheduledInstancesIAMInstanceProfile (Maybe Text)
siiapName = lens _siiapName (\s a -> s {_siiapName = a})

instance Hashable ScheduledInstancesIAMInstanceProfile

instance NFData ScheduledInstancesIAMInstanceProfile

instance ToQuery ScheduledInstancesIAMInstanceProfile where
  toQuery ScheduledInstancesIAMInstanceProfile' {..} =
    mconcat ["Arn" =: _siiapARN, "Name" =: _siiapName]
