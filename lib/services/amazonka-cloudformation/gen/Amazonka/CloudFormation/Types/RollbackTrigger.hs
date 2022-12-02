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
-- Module      : Amazonka.CloudFormation.Types.RollbackTrigger
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.RollbackTrigger where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A rollback trigger CloudFormation monitors during creation and updating
-- of stacks. If any of the alarms you specify goes to ALARM state during
-- the stack operation or within the specified monitoring period
-- afterwards, CloudFormation rolls back the entire stack operation.
--
-- /See:/ 'newRollbackTrigger' smart constructor.
data RollbackTrigger = RollbackTrigger'
  { -- | The Amazon Resource Name (ARN) of the rollback trigger.
    --
    -- If a specified trigger is missing, the entire stack operation fails and
    -- is rolled back.
    arn :: Prelude.Text,
    -- | The resource type of the rollback trigger. Specify either
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm>
    -- or
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cloudwatch-compositealarm.html AWS::CloudWatch::CompositeAlarm>
    -- resource types.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'type'', 'rollbackTrigger_type' - The resource type of the rollback trigger. Specify either
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm>
-- or
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cloudwatch-compositealarm.html AWS::CloudWatch::CompositeAlarm>
-- resource types.
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

-- | The resource type of the rollback trigger. Specify either
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-cw-alarm.html AWS::CloudWatch::Alarm>
-- or
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-cloudwatch-compositealarm.html AWS::CloudWatch::CompositeAlarm>
-- resource types.
rollbackTrigger_type :: Lens.Lens' RollbackTrigger Prelude.Text
rollbackTrigger_type = Lens.lens (\RollbackTrigger' {type'} -> type') (\s@RollbackTrigger' {} a -> s {type' = a} :: RollbackTrigger)

instance Data.FromXML RollbackTrigger where
  parseXML x =
    RollbackTrigger'
      Prelude.<$> (x Data..@ "Arn") Prelude.<*> (x Data..@ "Type")

instance Prelude.Hashable RollbackTrigger where
  hashWithSalt _salt RollbackTrigger' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RollbackTrigger where
  rnf RollbackTrigger' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf type'

instance Data.ToQuery RollbackTrigger where
  toQuery RollbackTrigger' {..} =
    Prelude.mconcat
      ["Arn" Data.=: arn, "Type" Data.=: type']
