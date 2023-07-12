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
-- Module      : Amazonka.EC2.Types.TargetGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes a load balancer target group.
--
-- /See:/ 'newTargetGroup' smart constructor.
data TargetGroup = TargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML TargetGroup where
  parseXML x =
    TargetGroup' Prelude.<$> (x Data..@? "arn")

instance Prelude.Hashable TargetGroup where
  hashWithSalt _salt TargetGroup' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData TargetGroup where
  rnf TargetGroup' {..} = Prelude.rnf arn

instance Data.ToQuery TargetGroup where
  toQuery TargetGroup' {..} =
    Prelude.mconcat ["Arn" Data.=: arn]
