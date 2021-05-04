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
-- Module      : Network.AWS.DeviceFarm.Types.ProblemDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ProblemDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a problem detail.
--
-- /See:/ 'newProblemDetail' smart constructor.
data ProblemDetail = ProblemDetail'
  { -- | The problem detail\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The problem detail\'s name.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProblemDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'problemDetail_arn' - The problem detail\'s ARN.
--
-- 'name', 'problemDetail_name' - The problem detail\'s name.
newProblemDetail ::
  ProblemDetail
newProblemDetail =
  ProblemDetail'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The problem detail\'s ARN.
problemDetail_arn :: Lens.Lens' ProblemDetail (Prelude.Maybe Prelude.Text)
problemDetail_arn = Lens.lens (\ProblemDetail' {arn} -> arn) (\s@ProblemDetail' {} a -> s {arn = a} :: ProblemDetail)

-- | The problem detail\'s name.
problemDetail_name :: Lens.Lens' ProblemDetail (Prelude.Maybe Prelude.Text)
problemDetail_name = Lens.lens (\ProblemDetail' {name} -> name) (\s@ProblemDetail' {} a -> s {name = a} :: ProblemDetail)

instance Prelude.FromJSON ProblemDetail where
  parseJSON =
    Prelude.withObject
      "ProblemDetail"
      ( \x ->
          ProblemDetail'
            Prelude.<$> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "name")
      )

instance Prelude.Hashable ProblemDetail

instance Prelude.NFData ProblemDetail
