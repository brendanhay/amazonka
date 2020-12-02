{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfileSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfileSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an IAM instance profile.
--
--
--
-- /See:/ 'iamInstanceProfileSpecification' smart constructor.
data IAMInstanceProfileSpecification = IAMInstanceProfileSpecification'
  { _iapsARN ::
      !(Maybe Text),
    _iapsName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IAMInstanceProfileSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapsARN' - The Amazon Resource Name (ARN) of the instance profile.
--
-- * 'iapsName' - The name of the instance profile.
iamInstanceProfileSpecification ::
  IAMInstanceProfileSpecification
iamInstanceProfileSpecification =
  IAMInstanceProfileSpecification'
    { _iapsARN = Nothing,
      _iapsName = Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iapsARN :: Lens' IAMInstanceProfileSpecification (Maybe Text)
iapsARN = lens _iapsARN (\s a -> s {_iapsARN = a})

-- | The name of the instance profile.
iapsName :: Lens' IAMInstanceProfileSpecification (Maybe Text)
iapsName = lens _iapsName (\s a -> s {_iapsName = a})

instance FromXML IAMInstanceProfileSpecification where
  parseXML x =
    IAMInstanceProfileSpecification'
      <$> (x .@? "arn") <*> (x .@? "name")

instance Hashable IAMInstanceProfileSpecification

instance NFData IAMInstanceProfileSpecification

instance ToQuery IAMInstanceProfileSpecification where
  toQuery IAMInstanceProfileSpecification' {..} =
    mconcat ["Arn" =: _iapsARN, "Name" =: _iapsName]
