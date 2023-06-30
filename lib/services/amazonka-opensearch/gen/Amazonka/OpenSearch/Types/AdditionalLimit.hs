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
-- Module      : Amazonka.OpenSearch.Types.AdditionalLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AdditionalLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of limits that are specific to a given instance type.
--
-- /See:/ 'newAdditionalLimit' smart constructor.
data AdditionalLimit = AdditionalLimit'
  { -- | -   @MaximumNumberOfDataNodesSupported@ - This attribute only applies to
    --     master nodes and specifies the maximum number of data nodes of a
    --     given instance type a master node can support.
    --
    -- -   @MaximumNumberOfDataNodesWithoutMasterNode@ - This attribute only
    --     applies to data nodes and specifies the maximum number of data nodes
    --     of a given instance type can exist without a master node governing
    --     them.
    limitName :: Prelude.Maybe Prelude.Text,
    -- | The values of the additional instance type limits.
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
-- 'limitName', 'additionalLimit_limitName' - -   @MaximumNumberOfDataNodesSupported@ - This attribute only applies to
--     master nodes and specifies the maximum number of data nodes of a
--     given instance type a master node can support.
--
-- -   @MaximumNumberOfDataNodesWithoutMasterNode@ - This attribute only
--     applies to data nodes and specifies the maximum number of data nodes
--     of a given instance type can exist without a master node governing
--     them.
--
-- 'limitValues', 'additionalLimit_limitValues' - The values of the additional instance type limits.
newAdditionalLimit ::
  AdditionalLimit
newAdditionalLimit =
  AdditionalLimit'
    { limitName = Prelude.Nothing,
      limitValues = Prelude.Nothing
    }

-- | -   @MaximumNumberOfDataNodesSupported@ - This attribute only applies to
--     master nodes and specifies the maximum number of data nodes of a
--     given instance type a master node can support.
--
-- -   @MaximumNumberOfDataNodesWithoutMasterNode@ - This attribute only
--     applies to data nodes and specifies the maximum number of data nodes
--     of a given instance type can exist without a master node governing
--     them.
additionalLimit_limitName :: Lens.Lens' AdditionalLimit (Prelude.Maybe Prelude.Text)
additionalLimit_limitName = Lens.lens (\AdditionalLimit' {limitName} -> limitName) (\s@AdditionalLimit' {} a -> s {limitName = a} :: AdditionalLimit)

-- | The values of the additional instance type limits.
additionalLimit_limitValues :: Lens.Lens' AdditionalLimit (Prelude.Maybe [Prelude.Text])
additionalLimit_limitValues = Lens.lens (\AdditionalLimit' {limitValues} -> limitValues) (\s@AdditionalLimit' {} a -> s {limitValues = a} :: AdditionalLimit) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AdditionalLimit where
  parseJSON =
    Data.withObject
      "AdditionalLimit"
      ( \x ->
          AdditionalLimit'
            Prelude.<$> (x Data..:? "LimitName")
            Prelude.<*> (x Data..:? "LimitValues" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AdditionalLimit where
  hashWithSalt _salt AdditionalLimit' {..} =
    _salt
      `Prelude.hashWithSalt` limitName
      `Prelude.hashWithSalt` limitValues

instance Prelude.NFData AdditionalLimit where
  rnf AdditionalLimit' {..} =
    Prelude.rnf limitName
      `Prelude.seq` Prelude.rnf limitValues
