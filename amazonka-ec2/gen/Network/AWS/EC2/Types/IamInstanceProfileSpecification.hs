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
-- Module      : Network.AWS.EC2.Types.IamInstanceProfileSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IamInstanceProfileSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an IAM instance profile.
--
-- /See:/ 'newIamInstanceProfileSpecification' smart constructor.
data IamInstanceProfileSpecification = IamInstanceProfileSpecification'
  { -- | The Amazon Resource Name (ARN) of the instance profile.
    arn :: Core.Maybe Core.Text,
    -- | The name of the instance profile.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'IamInstanceProfileSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'iamInstanceProfileSpecification_arn' - The Amazon Resource Name (ARN) of the instance profile.
--
-- 'name', 'iamInstanceProfileSpecification_name' - The name of the instance profile.
newIamInstanceProfileSpecification ::
  IamInstanceProfileSpecification
newIamInstanceProfileSpecification =
  IamInstanceProfileSpecification'
    { arn =
        Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance profile.
iamInstanceProfileSpecification_arn :: Lens.Lens' IamInstanceProfileSpecification (Core.Maybe Core.Text)
iamInstanceProfileSpecification_arn = Lens.lens (\IamInstanceProfileSpecification' {arn} -> arn) (\s@IamInstanceProfileSpecification' {} a -> s {arn = a} :: IamInstanceProfileSpecification)

-- | The name of the instance profile.
iamInstanceProfileSpecification_name :: Lens.Lens' IamInstanceProfileSpecification (Core.Maybe Core.Text)
iamInstanceProfileSpecification_name = Lens.lens (\IamInstanceProfileSpecification' {name} -> name) (\s@IamInstanceProfileSpecification' {} a -> s {name = a} :: IamInstanceProfileSpecification)

instance Core.FromXML IamInstanceProfileSpecification where
  parseXML x =
    IamInstanceProfileSpecification'
      Core.<$> (x Core..@? "arn") Core.<*> (x Core..@? "name")

instance
  Core.Hashable
    IamInstanceProfileSpecification

instance Core.NFData IamInstanceProfileSpecification

instance Core.ToQuery IamInstanceProfileSpecification where
  toQuery IamInstanceProfileSpecification' {..} =
    Core.mconcat
      ["Arn" Core.=: arn, "Name" Core.=: name]
