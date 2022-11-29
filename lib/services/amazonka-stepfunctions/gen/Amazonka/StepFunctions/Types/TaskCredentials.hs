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
-- Module      : Amazonka.StepFunctions.Types.TaskCredentials
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.TaskCredentials where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the credentials that Step Functions uses for a
-- task.
--
-- /See:/ 'newTaskCredentials' smart constructor.
data TaskCredentials = TaskCredentials'
  { -- | The ARN of an IAM role that Step Functions assumes for the task. The
    -- role can allow cross-account access to resources.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'taskCredentials_roleArn' - The ARN of an IAM role that Step Functions assumes for the task. The
-- role can allow cross-account access to resources.
newTaskCredentials ::
  TaskCredentials
newTaskCredentials =
  TaskCredentials' {roleArn = Prelude.Nothing}

-- | The ARN of an IAM role that Step Functions assumes for the task. The
-- role can allow cross-account access to resources.
taskCredentials_roleArn :: Lens.Lens' TaskCredentials (Prelude.Maybe Prelude.Text)
taskCredentials_roleArn = Lens.lens (\TaskCredentials' {roleArn} -> roleArn) (\s@TaskCredentials' {} a -> s {roleArn = a} :: TaskCredentials)

instance Core.FromJSON TaskCredentials where
  parseJSON =
    Core.withObject
      "TaskCredentials"
      ( \x ->
          TaskCredentials' Prelude.<$> (x Core..:? "roleArn")
      )

instance Prelude.Hashable TaskCredentials where
  hashWithSalt _salt TaskCredentials' {..} =
    _salt `Prelude.hashWithSalt` roleArn

instance Prelude.NFData TaskCredentials where
  rnf TaskCredentials' {..} = Prelude.rnf roleArn
