{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.Destination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a cross-account destination that receives subscription log events.
--
--
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dTargetARN :: !(Maybe Text),
    _dCreationTime :: !(Maybe Nat),
    _dArn :: !(Maybe Text),
    _dAccessPolicy :: !(Maybe Text),
    _dDestinationName :: !(Maybe Text),
    _dRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dTargetARN' - The Amazon Resource Name (ARN) of the physical target where the log events are delivered (for example, a Kinesis stream).
--
-- * 'dCreationTime' - The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- * 'dArn' - The ARN of this destination.
--
-- * 'dAccessPolicy' - An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
--
-- * 'dDestinationName' - The name of the destination.
--
-- * 'dRoleARN' - A role for impersonation, used when delivering log events to the target.
destination ::
  Destination
destination =
  Destination'
    { _dTargetARN = Nothing,
      _dCreationTime = Nothing,
      _dArn = Nothing,
      _dAccessPolicy = Nothing,
      _dDestinationName = Nothing,
      _dRoleARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the physical target where the log events are delivered (for example, a Kinesis stream).
dTargetARN :: Lens' Destination (Maybe Text)
dTargetARN = lens _dTargetARN (\s a -> s {_dTargetARN = a})

-- | The creation time of the destination, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC.
dCreationTime :: Lens' Destination (Maybe Natural)
dCreationTime = lens _dCreationTime (\s a -> s {_dCreationTime = a}) . mapping _Nat

-- | The ARN of this destination.
dArn :: Lens' Destination (Maybe Text)
dArn = lens _dArn (\s a -> s {_dArn = a})

-- | An IAM policy document that governs which AWS accounts can create subscription filters against this destination.
dAccessPolicy :: Lens' Destination (Maybe Text)
dAccessPolicy = lens _dAccessPolicy (\s a -> s {_dAccessPolicy = a})

-- | The name of the destination.
dDestinationName :: Lens' Destination (Maybe Text)
dDestinationName = lens _dDestinationName (\s a -> s {_dDestinationName = a})

-- | A role for impersonation, used when delivering log events to the target.
dRoleARN :: Lens' Destination (Maybe Text)
dRoleARN = lens _dRoleARN (\s a -> s {_dRoleARN = a})

instance FromJSON Destination where
  parseJSON =
    withObject
      "Destination"
      ( \x ->
          Destination'
            <$> (x .:? "targetArn")
            <*> (x .:? "creationTime")
            <*> (x .:? "arn")
            <*> (x .:? "accessPolicy")
            <*> (x .:? "destinationName")
            <*> (x .:? "roleArn")
      )

instance Hashable Destination

instance NFData Destination
