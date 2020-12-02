{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Transition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Transition where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.TransitionStorageClass

-- | Specifies when an object transitions to a specified storage class. For more information about Amazon S3 lifecycle configuration rules, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-transition-general-considerations.html Transitioning Objects Using Amazon S3 Lifecycle> in the /Amazon Simple Storage Service Developer Guide/ .
--
--
--
-- /See:/ 'transition' smart constructor.
data Transition = Transition'
  { _traDays :: !(Maybe Int),
    _traDate :: !(Maybe ISO8601),
    _traStorageClass :: !(Maybe TransitionStorageClass)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Transition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'traDays' - Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
--
-- * 'traDate' - Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
--
-- * 'traStorageClass' - The storage class to which you want the object to transition.
transition ::
  Transition
transition =
  Transition'
    { _traDays = Nothing,
      _traDate = Nothing,
      _traStorageClass = Nothing
    }

-- | Indicates the number of days after creation when objects are transitioned to the specified storage class. The value must be a positive integer.
traDays :: Lens' Transition (Maybe Int)
traDays = lens _traDays (\s a -> s {_traDays = a})

-- | Indicates when objects are transitioned to the specified storage class. The date value must be in ISO 8601 format. The time is always midnight UTC.
traDate :: Lens' Transition (Maybe UTCTime)
traDate = lens _traDate (\s a -> s {_traDate = a}) . mapping _Time

-- | The storage class to which you want the object to transition.
traStorageClass :: Lens' Transition (Maybe TransitionStorageClass)
traStorageClass = lens _traStorageClass (\s a -> s {_traStorageClass = a})

instance FromXML Transition where
  parseXML x =
    Transition'
      <$> (x .@? "Days") <*> (x .@? "Date") <*> (x .@? "StorageClass")

instance Hashable Transition

instance NFData Transition

instance ToXML Transition where
  toXML Transition' {..} =
    mconcat
      [ "Days" @= _traDays,
        "Date" @= _traDate,
        "StorageClass" @= _traStorageClass
      ]
