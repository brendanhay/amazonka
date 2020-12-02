{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftInformation where

import Network.AWS.CloudFormation.Types.StackDriftStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about whether the stack's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted.
--
--
--
-- /See:/ 'stackDriftInformation' smart constructor.
data StackDriftInformation = StackDriftInformation'
  { _sdiLastCheckTimestamp ::
      !(Maybe ISO8601),
    _sdiStackDriftStatus :: !StackDriftStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackDriftInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdiLastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
--
-- * 'sdiStackDriftStatus' - Status of the stack's actual configuration compared to its expected template configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
stackDriftInformation ::
  -- | 'sdiStackDriftStatus'
  StackDriftStatus ->
  StackDriftInformation
stackDriftInformation pStackDriftStatus_ =
  StackDriftInformation'
    { _sdiLastCheckTimestamp = Nothing,
      _sdiStackDriftStatus = pStackDriftStatus_
    }

-- | Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
sdiLastCheckTimestamp :: Lens' StackDriftInformation (Maybe UTCTime)
sdiLastCheckTimestamp = lens _sdiLastCheckTimestamp (\s a -> s {_sdiLastCheckTimestamp = a}) . mapping _Time

-- | Status of the stack's actual configuration compared to its expected template configuration.      * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.     * @UNKNOWN@ : This value is reserved for future use.
sdiStackDriftStatus :: Lens' StackDriftInformation StackDriftStatus
sdiStackDriftStatus = lens _sdiStackDriftStatus (\s a -> s {_sdiStackDriftStatus = a})

instance FromXML StackDriftInformation where
  parseXML x =
    StackDriftInformation'
      <$> (x .@? "LastCheckTimestamp") <*> (x .@ "StackDriftStatus")

instance Hashable StackDriftInformation

instance NFData StackDriftInformation
