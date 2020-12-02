{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftInformation where

import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
--
--
-- /See:/ 'stackResourceDriftInformation' smart constructor.
data StackResourceDriftInformation = StackResourceDriftInformation'
  { _srdiLastCheckTimestamp ::
      !(Maybe ISO8601),
    _srdiStackResourceDriftStatus ::
      !StackResourceDriftStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackResourceDriftInformation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdiLastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- * 'srdiStackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .      * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
stackResourceDriftInformation ::
  -- | 'srdiStackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformation
stackResourceDriftInformation pStackResourceDriftStatus_ =
  StackResourceDriftInformation'
    { _srdiLastCheckTimestamp = Nothing,
      _srdiStackResourceDriftStatus = pStackResourceDriftStatus_
    }

-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
srdiLastCheckTimestamp :: Lens' StackResourceDriftInformation (Maybe UTCTime)
srdiLastCheckTimestamp = lens _srdiLastCheckTimestamp (\s a -> s {_srdiLastCheckTimestamp = a}) . mapping _Time

-- | Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .      * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
srdiStackResourceDriftStatus :: Lens' StackResourceDriftInformation StackResourceDriftStatus
srdiStackResourceDriftStatus = lens _srdiStackResourceDriftStatus (\s a -> s {_srdiStackResourceDriftStatus = a})

instance FromXML StackResourceDriftInformation where
  parseXML x =
    StackResourceDriftInformation'
      <$> (x .@? "LastCheckTimestamp") <*> (x .@ "StackResourceDriftStatus")

instance Hashable StackResourceDriftInformation

instance NFData StackResourceDriftInformation
