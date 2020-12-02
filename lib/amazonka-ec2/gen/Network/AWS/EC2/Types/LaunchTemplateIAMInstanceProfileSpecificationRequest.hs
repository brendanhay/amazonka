{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateIAMInstanceProfileSpecificationRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An IAM instance profile.
--
--
--
-- /See:/ 'launchTemplateIAMInstanceProfileSpecificationRequest' smart constructor.
data LaunchTemplateIAMInstanceProfileSpecificationRequest = LaunchTemplateIAMInstanceProfileSpecificationRequest'
  { _ltiapsrARN ::
      !( Maybe
           Text
       ),
    _ltiapsrName ::
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

-- | Creates a value of 'LaunchTemplateIAMInstanceProfileSpecificationRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltiapsrARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'ltiapsrName' - The name of the instance profile.
launchTemplateIAMInstanceProfileSpecificationRequest ::
  LaunchTemplateIAMInstanceProfileSpecificationRequest
launchTemplateIAMInstanceProfileSpecificationRequest =
  LaunchTemplateIAMInstanceProfileSpecificationRequest'
    { _ltiapsrARN =
        Nothing,
      _ltiapsrName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
ltiapsrARN :: Lens' LaunchTemplateIAMInstanceProfileSpecificationRequest (Maybe Text)
ltiapsrARN = lens _ltiapsrARN (\s a -> s {_ltiapsrARN = a})

-- | The name of the instance profile.
ltiapsrName :: Lens' LaunchTemplateIAMInstanceProfileSpecificationRequest (Maybe Text)
ltiapsrName = lens _ltiapsrName (\s a -> s {_ltiapsrName = a})

instance
  Hashable
    LaunchTemplateIAMInstanceProfileSpecificationRequest

instance
  NFData
    LaunchTemplateIAMInstanceProfileSpecificationRequest

instance
  ToQuery
    LaunchTemplateIAMInstanceProfileSpecificationRequest
  where
  toQuery LaunchTemplateIAMInstanceProfileSpecificationRequest' {..} =
    mconcat ["Arn" =: _ltiapsrARN, "Name" =: _ltiapsrName]
