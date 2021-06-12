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
-- Module      : Network.AWS.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroup where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes a load balancer target group.
--
-- /See:/ 'newTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'targetGroup_arn' - The Amazon Resource Name (ARN) of the target group.
newTargetGroup ::
  TargetGroup
newTargetGroup = TargetGroup' {arn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
targetGroup_arn :: Lens.Lens' TargetGroup (Core.Maybe Core.Text)
targetGroup_arn = Lens.lens (\TargetGroup' {arn} -> arn) (\s@TargetGroup' {} a -> s {arn = a} :: TargetGroup)

instance Core.FromXML TargetGroup where
  parseXML x = TargetGroup' Core.<$> (x Core..@? "arn")

instance Core.Hashable TargetGroup

instance Core.NFData TargetGroup

instance Core.ToQuery TargetGroup where
  toQuery TargetGroup' {..} =
    Core.mconcat ["Arn" Core.=: arn]
