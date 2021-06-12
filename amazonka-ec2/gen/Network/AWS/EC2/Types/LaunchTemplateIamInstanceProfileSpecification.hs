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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateIamInstanceProfileSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an IAM instance profile.
--
-- /See:/ 'newLaunchTemplateIamInstanceProfileSpecification' smart constructor.
data LaunchTemplateIamInstanceProfileSpecification = LaunchTemplateIamInstanceProfileSpecification'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Core.Maybe Core.Text,
    -- | The name of the instance profile.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateIamInstanceProfileSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'launchTemplateIamInstanceProfileSpecification_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'launchTemplateIamInstanceProfileSpecification_name' - The name of the instance profile.
newLaunchTemplateIamInstanceProfileSpecification ::
  LaunchTemplateIamInstanceProfileSpecification
newLaunchTemplateIamInstanceProfileSpecification =
  LaunchTemplateIamInstanceProfileSpecification'
    { arn =
        Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
launchTemplateIamInstanceProfileSpecification_arn :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecification (Core.Maybe Core.Text)
launchTemplateIamInstanceProfileSpecification_arn = Lens.lens (\LaunchTemplateIamInstanceProfileSpecification' {arn} -> arn) (\s@LaunchTemplateIamInstanceProfileSpecification' {} a -> s {arn = a} :: LaunchTemplateIamInstanceProfileSpecification)

-- | The name of the instance profile.
launchTemplateIamInstanceProfileSpecification_name :: Lens.Lens' LaunchTemplateIamInstanceProfileSpecification (Core.Maybe Core.Text)
launchTemplateIamInstanceProfileSpecification_name = Lens.lens (\LaunchTemplateIamInstanceProfileSpecification' {name} -> name) (\s@LaunchTemplateIamInstanceProfileSpecification' {} a -> s {name = a} :: LaunchTemplateIamInstanceProfileSpecification)

instance
  Core.FromXML
    LaunchTemplateIamInstanceProfileSpecification
  where
  parseXML x =
    LaunchTemplateIamInstanceProfileSpecification'
      Core.<$> (x Core..@? "arn") Core.<*> (x Core..@? "name")

instance
  Core.Hashable
    LaunchTemplateIamInstanceProfileSpecification

instance
  Core.NFData
    LaunchTemplateIamInstanceProfileSpecification
