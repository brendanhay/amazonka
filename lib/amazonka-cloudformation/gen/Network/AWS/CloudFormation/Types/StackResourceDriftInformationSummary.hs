{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary where

import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Summarizes information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
--
--
-- /See:/ 'stackResourceDriftInformationSummary' smart constructor.
data StackResourceDriftInformationSummary = StackResourceDriftInformationSummary'
  { _srdisLastCheckTimestamp ::
      !(Maybe ISO8601),
    _srdisStackResourceDriftStatus ::
      !StackResourceDriftStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackResourceDriftInformationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdisLastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- * 'srdisStackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> . If you performed an 'ContinueUpdateRollback' operation on a stack, any resources included in @ResourcesToSkip@ will also have a status of @NOT_CHECKED@ . For more information on skipping resources during rollback operations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update> in the AWS CloudFormation User Guide.     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
stackResourceDriftInformationSummary ::
  -- | 'srdisStackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformationSummary
stackResourceDriftInformationSummary pStackResourceDriftStatus_ =
  StackResourceDriftInformationSummary'
    { _srdisLastCheckTimestamp =
        Nothing,
      _srdisStackResourceDriftStatus =
        pStackResourceDriftStatus_
    }

-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
srdisLastCheckTimestamp :: Lens' StackResourceDriftInformationSummary (Maybe UTCTime)
srdisLastCheckTimestamp = lens _srdisLastCheckTimestamp (\s a -> s {_srdisLastCheckTimestamp = a}) . mapping _Time

-- | Status of the resource's actual configuration compared to its expected configuration     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.     * @MODIFIED@ : The resource differs from its expected configuration.     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration. Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> . If you performed an 'ContinueUpdateRollback' operation on a stack, any resources included in @ResourcesToSkip@ will also have a status of @NOT_CHECKED@ . For more information on skipping resources during rollback operations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update> in the AWS CloudFormation User Guide.     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
srdisStackResourceDriftStatus :: Lens' StackResourceDriftInformationSummary StackResourceDriftStatus
srdisStackResourceDriftStatus = lens _srdisStackResourceDriftStatus (\s a -> s {_srdisStackResourceDriftStatus = a})

instance FromXML StackResourceDriftInformationSummary where
  parseXML x =
    StackResourceDriftInformationSummary'
      <$> (x .@? "LastCheckTimestamp") <*> (x .@ "StackResourceDriftStatus")

instance Hashable StackResourceDriftInformationSummary

instance NFData StackResourceDriftInformationSummary
