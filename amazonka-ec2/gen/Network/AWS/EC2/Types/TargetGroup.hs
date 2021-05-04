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
-- Module      : Network.AWS.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetGroup where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a load balancer target group.
--
-- /See:/ 'newTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newTargetGroup = TargetGroup' {arn = Prelude.Nothing}

-- | The Amazon Resource Name (ARN) of the target group.
targetGroup_arn :: Lens.Lens' TargetGroup (Prelude.Maybe Prelude.Text)
targetGroup_arn = Lens.lens (\TargetGroup' {arn} -> arn) (\s@TargetGroup' {} a -> s {arn = a} :: TargetGroup)

instance Prelude.FromXML TargetGroup where
  parseXML x =
    TargetGroup' Prelude.<$> (x Prelude..@? "arn")

instance Prelude.Hashable TargetGroup

instance Prelude.NFData TargetGroup

instance Prelude.ToQuery TargetGroup where
  toQuery TargetGroup' {..} =
    Prelude.mconcat ["Arn" Prelude.=: arn]
