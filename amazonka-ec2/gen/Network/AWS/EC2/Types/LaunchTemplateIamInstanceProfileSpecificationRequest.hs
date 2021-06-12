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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecificationRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | An IAM instance profile.
--
-- /See:/ 'newLaunchTemplateIamInstanceProfileSpecificationRequest' smart constructor.
data LaunchTemplateIamInstanceProfileSpecificationRequest = LaunchTemplateIamInstanceProfileSpecificationRequest'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Core.Maybe Core.Text,
    -- | The name of the instance profile.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateIamInstanceProfileSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'launchTemplateIamInstanceProfileSpecificationRequest_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'launchTemplateIamInstanceProfileSpecificationRequest_name' - The name of the instance profile.
newLaunchTemplateIamInstanceProfileSpecificationRequest ::
  LaunchTemplateIamInstanceProfileSpecificationRequest
newLaunchTemplateIamInstanceProfileSpecificationRequest =
  LaunchTemplateIamInstanceProfileSpecificationRequest'
    { arn =
        Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
launchTemplateIamInstanceProfileSpecificationRequest_arn :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecificationRequest (Core.Maybe Core.Text)
launchTemplateIamInstanceProfileSpecificationRequest_arn = Lens.lens (\LaunchTemplateIamInstanceProfileSpecificationRequest' {arn} -> arn) (\s@LaunchTemplateIamInstanceProfileSpecificationRequest' {} a -> s {arn = a} :: LaunchTemplateIamInstanceProfileSpecificationRequest)

-- | The name of the instance profile.
launchTemplateIamInstanceProfileSpecificationRequest_name :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecificationRequest (Core.Maybe Core.Text)
launchTemplateIamInstanceProfileSpecificationRequest_name = Lens.lens (\LaunchTemplateIamInstanceProfileSpecificationRequest' {name} -> name) (\s@LaunchTemplateIamInstanceProfileSpecificationRequest' {} a -> s {name = a} :: LaunchTemplateIamInstanceProfileSpecificationRequest)

instance
  Core.Hashable
    LaunchTemplateIamInstanceProfileSpecificationRequest

instance
  Core.NFData
    LaunchTemplateIamInstanceProfileSpecificationRequest

instance
  Core.ToQuery
    LaunchTemplateIamInstanceProfileSpecificationRequest
  where
  toQuery
    LaunchTemplateIamInstanceProfileSpecificationRequest' {..} =
      Core.mconcat
        ["Arn" Core.=: arn, "Name" Core.=: name]
