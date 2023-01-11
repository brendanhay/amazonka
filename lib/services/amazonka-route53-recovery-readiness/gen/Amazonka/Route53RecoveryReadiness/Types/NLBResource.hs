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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.NLBResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.NLBResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Network Load Balancer resource that a DNS target resource points to.
--
-- /See:/ 'newNLBResource' smart constructor.
data NLBResource = NLBResource'
  { -- | The Network Load Balancer resource Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NLBResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'nLBResource_arn' - The Network Load Balancer resource Amazon Resource Name (ARN).
newNLBResource ::
  NLBResource
newNLBResource = NLBResource' {arn = Prelude.Nothing}

-- | The Network Load Balancer resource Amazon Resource Name (ARN).
nLBResource_arn :: Lens.Lens' NLBResource (Prelude.Maybe Prelude.Text)
nLBResource_arn = Lens.lens (\NLBResource' {arn} -> arn) (\s@NLBResource' {} a -> s {arn = a} :: NLBResource)

instance Data.FromJSON NLBResource where
  parseJSON =
    Data.withObject
      "NLBResource"
      (\x -> NLBResource' Prelude.<$> (x Data..:? "arn"))

instance Prelude.Hashable NLBResource where
  hashWithSalt _salt NLBResource' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData NLBResource where
  rnf NLBResource' {..} = Prelude.rnf arn

instance Data.ToJSON NLBResource where
  toJSON NLBResource' {..} =
    Data.object
      (Prelude.catMaybes [("arn" Data..=) Prelude.<$> arn])
