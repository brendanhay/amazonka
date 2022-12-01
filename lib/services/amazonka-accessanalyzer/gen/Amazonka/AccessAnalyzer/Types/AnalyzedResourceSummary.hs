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
-- Module      : Amazonka.AccessAnalyzer.Types.AnalyzedResourceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.AnalyzedResourceSummary where

import Amazonka.AccessAnalyzer.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the ARN of the analyzed resource.
--
-- /See:/ 'newAnalyzedResourceSummary' smart constructor.
data AnalyzedResourceSummary = AnalyzedResourceSummary'
  { -- | The ARN of the analyzed resource.
    resourceArn :: Prelude.Text,
    -- | The Amazon Web Services account ID that owns the resource.
    resourceOwnerAccount :: Prelude.Text,
    -- | The type of resource that was analyzed.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzedResourceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'analyzedResourceSummary_resourceArn' - The ARN of the analyzed resource.
--
-- 'resourceOwnerAccount', 'analyzedResourceSummary_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource.
--
-- 'resourceType', 'analyzedResourceSummary_resourceType' - The type of resource that was analyzed.
newAnalyzedResourceSummary ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourceOwnerAccount'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  AnalyzedResourceSummary
newAnalyzedResourceSummary
  pResourceArn_
  pResourceOwnerAccount_
  pResourceType_ =
    AnalyzedResourceSummary'
      { resourceArn =
          pResourceArn_,
        resourceOwnerAccount = pResourceOwnerAccount_,
        resourceType = pResourceType_
      }

-- | The ARN of the analyzed resource.
analyzedResourceSummary_resourceArn :: Lens.Lens' AnalyzedResourceSummary Prelude.Text
analyzedResourceSummary_resourceArn = Lens.lens (\AnalyzedResourceSummary' {resourceArn} -> resourceArn) (\s@AnalyzedResourceSummary' {} a -> s {resourceArn = a} :: AnalyzedResourceSummary)

-- | The Amazon Web Services account ID that owns the resource.
analyzedResourceSummary_resourceOwnerAccount :: Lens.Lens' AnalyzedResourceSummary Prelude.Text
analyzedResourceSummary_resourceOwnerAccount = Lens.lens (\AnalyzedResourceSummary' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@AnalyzedResourceSummary' {} a -> s {resourceOwnerAccount = a} :: AnalyzedResourceSummary)

-- | The type of resource that was analyzed.
analyzedResourceSummary_resourceType :: Lens.Lens' AnalyzedResourceSummary ResourceType
analyzedResourceSummary_resourceType = Lens.lens (\AnalyzedResourceSummary' {resourceType} -> resourceType) (\s@AnalyzedResourceSummary' {} a -> s {resourceType = a} :: AnalyzedResourceSummary)

instance Core.FromJSON AnalyzedResourceSummary where
  parseJSON =
    Core.withObject
      "AnalyzedResourceSummary"
      ( \x ->
          AnalyzedResourceSummary'
            Prelude.<$> (x Core..: "resourceArn")
            Prelude.<*> (x Core..: "resourceOwnerAccount")
            Prelude.<*> (x Core..: "resourceType")
      )

instance Prelude.Hashable AnalyzedResourceSummary where
  hashWithSalt _salt AnalyzedResourceSummary' {..} =
    _salt `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceOwnerAccount
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData AnalyzedResourceSummary where
  rnf AnalyzedResourceSummary' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceOwnerAccount
      `Prelude.seq` Prelude.rnf resourceType
