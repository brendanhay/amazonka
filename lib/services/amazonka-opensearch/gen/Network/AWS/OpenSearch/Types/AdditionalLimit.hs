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
-- Module      : Network.AWS.OpenSearch.Types.AdditionalLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpenSearch.Types.AdditionalLimit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of limits that are specific to a given InstanceType and for each of
-- its @ InstanceRole @ .
--
-- /See:/ 'newAdditionalLimit' smart constructor.
data AdditionalLimit = AdditionalLimit'
  { -- | Additional limit is specific to a given InstanceType and for each of its
    -- @ InstanceRole @ etc.
    -- Attributes and their details:
    --
    -- -   MaximumNumberOfDataNodesSupported
    -- -   MaximumNumberOfDataNodesWithoutMasterNode
    limitName :: Prelude.Maybe Prelude.Text,
    -- | Value for a given @ AdditionalLimit$LimitName @ .
    limitValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdditionalLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitName', 'additionalLimit_limitName' - Additional limit is specific to a given InstanceType and for each of its
-- @ InstanceRole @ etc.
-- Attributes and their details:
--
-- -   MaximumNumberOfDataNodesSupported
-- -   MaximumNumberOfDataNodesWithoutMasterNode
--
-- 'limitValues', 'additionalLimit_limitValues' - Value for a given @ AdditionalLimit$LimitName @ .
newAdditionalLimit ::
  AdditionalLimit
newAdditionalLimit =
  AdditionalLimit'
    { limitName = Prelude.Nothing,
      limitValues = Prelude.Nothing
    }

-- | Additional limit is specific to a given InstanceType and for each of its
-- @ InstanceRole @ etc.
-- Attributes and their details:
--
-- -   MaximumNumberOfDataNodesSupported
-- -   MaximumNumberOfDataNodesWithoutMasterNode
additionalLimit_limitName :: Lens.Lens' AdditionalLimit (Prelude.Maybe Prelude.Text)
additionalLimit_limitName = Lens.lens (\AdditionalLimit' {limitName} -> limitName) (\s@AdditionalLimit' {} a -> s {limitName = a} :: AdditionalLimit)

-- | Value for a given @ AdditionalLimit$LimitName @ .
additionalLimit_limitValues :: Lens.Lens' AdditionalLimit (Prelude.Maybe [Prelude.Text])
additionalLimit_limitValues = Lens.lens (\AdditionalLimit' {limitValues} -> limitValues) (\s@AdditionalLimit' {} a -> s {limitValues = a} :: AdditionalLimit) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AdditionalLimit where
  parseJSON =
    Core.withObject
      "AdditionalLimit"
      ( \x ->
          AdditionalLimit'
            Prelude.<$> (x Core..:? "LimitName")
            Prelude.<*> (x Core..:? "LimitValues" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AdditionalLimit

instance Prelude.NFData AdditionalLimit
