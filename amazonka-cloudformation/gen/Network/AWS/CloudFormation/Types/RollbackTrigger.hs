{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.RollbackTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.RollbackTrigger where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A rollback trigger AWS CloudFormation monitors during creation and
-- updating of stacks. If any of the alarms you specify goes to ALARM state
-- during the stack operation or within the specified monitoring period
-- afterwards, CloudFormation rolls back the entire stack operation.
--
-- /See:/ 'newRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { -- | The Amazon Resource Name (ARN) of the rollback trigger.
    --
    -- If a specified trigger is missing, the entire stack operation fails and
    -- is rolled back.
    arn :: Prelude.Text,
    -- | The resource type of the rollback trigger. Currently,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm>
    -- is the only supported resource type.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RollbackTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'rollbackTrigger_arn' - The Amazon Resource Name (ARN) of the rollback trigger.
--
-- If a specified trigger is missing, the entire stack operation fails and
-- is rolled back.
--
-- 'type'', 'rollbackTrigger_type' - The resource type of the rollback trigger. Currently,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm>
-- is the only supported resource type.
newRollbackTrigger ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  RollbackTrigger
newRollbackTrigger pArn_ pType_ =
  RollbackTrigger' {arn = pArn_, type' = pType_}

-- | The Amazon Resource Name (ARN) of the rollback trigger.
--
-- If a specified trigger is missing, the entire stack operation fails and
-- is rolled back.
rollbackTrigger_arn :: Lens.Lens' RollbackTrigger Prelude.Text
rollbackTrigger_arn = Lens.lens (\RollbackTrigger' {arn} -> arn) (\s@RollbackTrigger' {} a -> s {arn = a} :: RollbackTrigger)

-- | The resource type of the rollback trigger. Currently,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm>
-- is the only supported resource type.
rollbackTrigger_type :: Lens.Lens' RollbackTrigger Prelude.Text
rollbackTrigger_type = Lens.lens (\RollbackTrigger' {type'} -> type') (\s@RollbackTrigger' {} a -> s {type' = a} :: RollbackTrigger)

instance Prelude.FromXML RollbackTrigger where
  parseXML x =
    RollbackTrigger'
      Prelude.<$> (x Prelude..@ "Arn")
      Prelude.<*> (x Prelude..@ "Type")

instance Prelude.Hashable RollbackTrigger

instance Prelude.NFData RollbackTrigger

instance Prelude.ToQuery RollbackTrigger where
  toQuery RollbackTrigger' {..} =
    Prelude.mconcat
      ["Arn" Prelude.=: arn, "Type" Prelude.=: type']
