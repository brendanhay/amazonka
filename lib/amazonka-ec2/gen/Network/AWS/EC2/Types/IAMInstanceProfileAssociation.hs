{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IAMInstanceProfileAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IAMInstanceProfileAssociation where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.IAMInstanceProfile
import Network.AWS.EC2.Types.IAMInstanceProfileAssociationState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an association between an IAM instance profile and an instance.
--
--
--
-- /See:/ 'iamInstanceProfileAssociation' smart constructor.
data IAMInstanceProfileAssociation = IAMInstanceProfileAssociation'
  { _iapaAssociationId ::
      !(Maybe Text),
    _iapaInstanceId ::
      !(Maybe Text),
    _iapaState ::
      !( Maybe
           IAMInstanceProfileAssociationState
       ),
    _iapaIAMInstanceProfile ::
      !(Maybe IAMInstanceProfile),
    _iapaTimestamp ::
      !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IAMInstanceProfileAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iapaAssociationId' - The ID of the association.
--
-- * 'iapaInstanceId' - The ID of the instance.
--
-- * 'iapaState' - The state of the association.
--
-- * 'iapaIAMInstanceProfile' - The IAM instance profile.
--
-- * 'iapaTimestamp' - The time the IAM instance profile was associated with the instance.
iamInstanceProfileAssociation ::
  IAMInstanceProfileAssociation
iamInstanceProfileAssociation =
  IAMInstanceProfileAssociation'
    { _iapaAssociationId = Nothing,
      _iapaInstanceId = Nothing,
      _iapaState = Nothing,
      _iapaIAMInstanceProfile = Nothing,
      _iapaTimestamp = Nothing
    }

-- | The ID of the association.
iapaAssociationId :: Lens' IAMInstanceProfileAssociation (Maybe Text)
iapaAssociationId = lens _iapaAssociationId (\s a -> s {_iapaAssociationId = a})

-- | The ID of the instance.
iapaInstanceId :: Lens' IAMInstanceProfileAssociation (Maybe Text)
iapaInstanceId = lens _iapaInstanceId (\s a -> s {_iapaInstanceId = a})

-- | The state of the association.
iapaState :: Lens' IAMInstanceProfileAssociation (Maybe IAMInstanceProfileAssociationState)
iapaState = lens _iapaState (\s a -> s {_iapaState = a})

-- | The IAM instance profile.
iapaIAMInstanceProfile :: Lens' IAMInstanceProfileAssociation (Maybe IAMInstanceProfile)
iapaIAMInstanceProfile = lens _iapaIAMInstanceProfile (\s a -> s {_iapaIAMInstanceProfile = a})

-- | The time the IAM instance profile was associated with the instance.
iapaTimestamp :: Lens' IAMInstanceProfileAssociation (Maybe UTCTime)
iapaTimestamp = lens _iapaTimestamp (\s a -> s {_iapaTimestamp = a}) . mapping _Time

instance FromXML IAMInstanceProfileAssociation where
  parseXML x =
    IAMInstanceProfileAssociation'
      <$> (x .@? "associationId")
      <*> (x .@? "instanceId")
      <*> (x .@? "state")
      <*> (x .@? "iamInstanceProfile")
      <*> (x .@? "timestamp")

instance Hashable IAMInstanceProfileAssociation

instance NFData IAMInstanceProfileAssociation
