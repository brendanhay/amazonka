{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.FailureDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.FailureDescription where

import Network.AWS.Firehose.Types.DeliveryStreamFailureType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
--
--
-- /See:/ 'failureDescription' smart constructor.
data FailureDescription = FailureDescription'
  { _fdType ::
      !DeliveryStreamFailureType,
    _fdDetails :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FailureDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdType' - The type of error that caused the failure.
--
-- * 'fdDetails' - A message providing details about the error that caused the failure.
failureDescription ::
  -- | 'fdType'
  DeliveryStreamFailureType ->
  -- | 'fdDetails'
  Text ->
  FailureDescription
failureDescription pType_ pDetails_ =
  FailureDescription' {_fdType = pType_, _fdDetails = pDetails_}

-- | The type of error that caused the failure.
fdType :: Lens' FailureDescription DeliveryStreamFailureType
fdType = lens _fdType (\s a -> s {_fdType = a})

-- | A message providing details about the error that caused the failure.
fdDetails :: Lens' FailureDescription Text
fdDetails = lens _fdDetails (\s a -> s {_fdDetails = a})

instance FromJSON FailureDescription where
  parseJSON =
    withObject
      "FailureDescription"
      (\x -> FailureDescription' <$> (x .: "Type") <*> (x .: "Details"))

instance Hashable FailureDescription

instance NFData FailureDescription
