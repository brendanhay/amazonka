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
-- Module      : Amazonka.XRay.Types.ResourceARNDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ResourceARNDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of resources ARNs corresponding to the segments in a trace.
--
-- /See:/ 'newResourceARNDetail' smart constructor.
data ResourceARNDetail = ResourceARNDetail'
  { -- | The ARN of a corresponding resource.
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceARNDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'resourceARNDetail_arn' - The ARN of a corresponding resource.
newResourceARNDetail ::
  ResourceARNDetail
newResourceARNDetail =
  ResourceARNDetail' {arn = Prelude.Nothing}

-- | The ARN of a corresponding resource.
resourceARNDetail_arn :: Lens.Lens' ResourceARNDetail (Prelude.Maybe Prelude.Text)
resourceARNDetail_arn = Lens.lens (\ResourceARNDetail' {arn} -> arn) (\s@ResourceARNDetail' {} a -> s {arn = a} :: ResourceARNDetail)

instance Core.FromJSON ResourceARNDetail where
  parseJSON =
    Core.withObject
      "ResourceARNDetail"
      ( \x ->
          ResourceARNDetail' Prelude.<$> (x Core..:? "ARN")
      )

instance Prelude.Hashable ResourceARNDetail where
  hashWithSalt _salt ResourceARNDetail' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData ResourceARNDetail where
  rnf ResourceARNDetail' {..} = Prelude.rnf arn
