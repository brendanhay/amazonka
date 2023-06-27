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
-- Module      : Amazonka.AccessAnalyzer.Types.FindingSourceDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.FindingSourceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Includes details about how the access that generated the finding is
-- granted. This is populated for Amazon S3 bucket findings.
--
-- /See:/ 'newFindingSourceDetail' smart constructor.
data FindingSourceDetail = FindingSourceDetail'
  { -- | The account of the cross-account access point that generated the
    -- finding.
    accessPointAccount :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the access point that generated the finding. The ARN format
    -- depends on whether the ARN represents an access point or a multi-region
    -- access point.
    accessPointArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingSourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPointAccount', 'findingSourceDetail_accessPointAccount' - The account of the cross-account access point that generated the
-- finding.
--
-- 'accessPointArn', 'findingSourceDetail_accessPointArn' - The ARN of the access point that generated the finding. The ARN format
-- depends on whether the ARN represents an access point or a multi-region
-- access point.
newFindingSourceDetail ::
  FindingSourceDetail
newFindingSourceDetail =
  FindingSourceDetail'
    { accessPointAccount =
        Prelude.Nothing,
      accessPointArn = Prelude.Nothing
    }

-- | The account of the cross-account access point that generated the
-- finding.
findingSourceDetail_accessPointAccount :: Lens.Lens' FindingSourceDetail (Prelude.Maybe Prelude.Text)
findingSourceDetail_accessPointAccount = Lens.lens (\FindingSourceDetail' {accessPointAccount} -> accessPointAccount) (\s@FindingSourceDetail' {} a -> s {accessPointAccount = a} :: FindingSourceDetail)

-- | The ARN of the access point that generated the finding. The ARN format
-- depends on whether the ARN represents an access point or a multi-region
-- access point.
findingSourceDetail_accessPointArn :: Lens.Lens' FindingSourceDetail (Prelude.Maybe Prelude.Text)
findingSourceDetail_accessPointArn = Lens.lens (\FindingSourceDetail' {accessPointArn} -> accessPointArn) (\s@FindingSourceDetail' {} a -> s {accessPointArn = a} :: FindingSourceDetail)

instance Data.FromJSON FindingSourceDetail where
  parseJSON =
    Data.withObject
      "FindingSourceDetail"
      ( \x ->
          FindingSourceDetail'
            Prelude.<$> (x Data..:? "accessPointAccount")
            Prelude.<*> (x Data..:? "accessPointArn")
      )

instance Prelude.Hashable FindingSourceDetail where
  hashWithSalt _salt FindingSourceDetail' {..} =
    _salt
      `Prelude.hashWithSalt` accessPointAccount
      `Prelude.hashWithSalt` accessPointArn

instance Prelude.NFData FindingSourceDetail where
  rnf FindingSourceDetail' {..} =
    Prelude.rnf accessPointAccount
      `Prelude.seq` Prelude.rnf accessPointArn
