{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IAM instance profile.
--
--
--
-- /See:/ 'launchTemplateIAMInstanceProfileSpecification' smart constructor.
data LaunchTemplateIAMInstanceProfileSpecification = LaunchTemplateIAMInstanceProfileSpecification'
  { _ltiapsARN ::
      !( Maybe
           Text
       ),
    _ltiapsName ::
      !( Maybe
           Text
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'LaunchTemplateIAMInstanceProfileSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltiapsARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'ltiapsName' - The name of the instance profile.
launchTemplateIAMInstanceProfileSpecification ::
  LaunchTemplateIAMInstanceProfileSpecification
launchTemplateIAMInstanceProfileSpecification =
  LaunchTemplateIAMInstanceProfileSpecification'
    { _ltiapsARN =
        Nothing,
      _ltiapsName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
ltiapsARN :: Lens' LaunchTemplateIAMInstanceProfileSpecification (Maybe Text)
ltiapsARN = lens _ltiapsARN (\s a -> s {_ltiapsARN = a})

-- | The name of the instance profile.
ltiapsName :: Lens' LaunchTemplateIAMInstanceProfileSpecification (Maybe Text)
ltiapsName = lens _ltiapsName (\s a -> s {_ltiapsName = a})

instance FromXML LaunchTemplateIAMInstanceProfileSpecification where
  parseXML x =
    LaunchTemplateIAMInstanceProfileSpecification'
      <$> (x .@? "arn") <*> (x .@? "name")

instance Hashable LaunchTemplateIAMInstanceProfileSpecification

instance NFData LaunchTemplateIAMInstanceProfileSpecification
