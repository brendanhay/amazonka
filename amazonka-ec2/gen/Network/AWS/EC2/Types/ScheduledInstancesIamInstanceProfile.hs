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
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesIamInstanceProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesIamInstanceProfile where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an IAM instance profile for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesIamInstanceProfile' smart constructor.
data ScheduledInstancesIamInstanceProfile = ScheduledInstancesIamInstanceProfile'
  { -- | The Amazon Resource Name (ARN).
    arn :: Core.Maybe Core.Text,
    -- | The name.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ScheduledInstancesIamInstanceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'scheduledInstancesIamInstanceProfile_arn' - The Amazon Resource Name (ARN).
--
-- 'name', 'scheduledInstancesIamInstanceProfile_name' - The name.
newScheduledInstancesIamInstanceProfile ::
  ScheduledInstancesIamInstanceProfile
newScheduledInstancesIamInstanceProfile =
  ScheduledInstancesIamInstanceProfile'
    { arn =
        Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN).
scheduledInstancesIamInstanceProfile_arn :: Lens.Lens' ScheduledInstancesIamInstanceProfile (Core.Maybe Core.Text)
scheduledInstancesIamInstanceProfile_arn = Lens.lens (\ScheduledInstancesIamInstanceProfile' {arn} -> arn) (\s@ScheduledInstancesIamInstanceProfile' {} a -> s {arn = a} :: ScheduledInstancesIamInstanceProfile)

-- | The name.
scheduledInstancesIamInstanceProfile_name :: Lens.Lens' ScheduledInstancesIamInstanceProfile (Core.Maybe Core.Text)
scheduledInstancesIamInstanceProfile_name = Lens.lens (\ScheduledInstancesIamInstanceProfile' {name} -> name) (\s@ScheduledInstancesIamInstanceProfile' {} a -> s {name = a} :: ScheduledInstancesIamInstanceProfile)

instance
  Core.Hashable
    ScheduledInstancesIamInstanceProfile

instance
  Core.NFData
    ScheduledInstancesIamInstanceProfile

instance
  Core.ToQuery
    ScheduledInstancesIamInstanceProfile
  where
  toQuery ScheduledInstancesIamInstanceProfile' {..} =
    Core.mconcat
      ["Arn" Core.=: arn, "Name" Core.=: name]
