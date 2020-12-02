{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RollbackTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RollbackTrigger where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A rollback trigger AWS CloudFormation monitors during creation and updating of stacks. If any of the alarms you specify goes to ALARM state during the stack operation or within the specified monitoring period afterwards, CloudFormation rolls back the entire stack operation.
--
--
--
-- /See:/ 'rollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { _rtARN :: !Text,
    _rtType :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RollbackTrigger' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtARN' - The Amazon Resource Name (ARN) of the rollback trigger. If a specified trigger is missing, the entire stack operation fails and is rolled back.
--
-- * 'rtType' - The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
rollbackTrigger ::
  -- | 'rtARN'
  Text ->
  -- | 'rtType'
  Text ->
  RollbackTrigger
rollbackTrigger pARN_ pType_ =
  RollbackTrigger' {_rtARN = pARN_, _rtType = pType_}

-- | The Amazon Resource Name (ARN) of the rollback trigger. If a specified trigger is missing, the entire stack operation fails and is rolled back.
rtARN :: Lens' RollbackTrigger Text
rtARN = lens _rtARN (\s a -> s {_rtARN = a})

-- | The resource type of the rollback trigger. Currently, <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm> is the only supported resource type.
rtType :: Lens' RollbackTrigger Text
rtType = lens _rtType (\s a -> s {_rtType = a})

instance FromXML RollbackTrigger where
  parseXML x = RollbackTrigger' <$> (x .@ "Arn") <*> (x .@ "Type")

instance Hashable RollbackTrigger

instance NFData RollbackTrigger

instance ToQuery RollbackTrigger where
  toQuery RollbackTrigger' {..} =
    mconcat ["Arn" =: _rtARN, "Type" =: _rtType]
