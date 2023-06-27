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
-- Module      : Amazonka.SecurityHub.Types.FindingHistoryUpdateSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingHistoryUpdateSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.FindingHistoryUpdateSourceType

-- | Identifies the source of the finding change event.
--
-- /See:/ 'newFindingHistoryUpdateSource' smart constructor.
data FindingHistoryUpdateSource = FindingHistoryUpdateSource'
  { -- | The identity of the source that initiated the finding change event. For
    -- example, the Amazon Resource Name (ARN) of a partner that calls
    -- BatchImportFindings or of a customer that calls BatchUpdateFindings.
    identity :: Prelude.Maybe Prelude.Text,
    -- | Describes the type of finding change event, such as a call to
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
    -- (by an integrated Amazon Web Service or third party partner integration)
    -- or
    -- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
    -- (by a Security Hub customer).
    type' :: Prelude.Maybe FindingHistoryUpdateSourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingHistoryUpdateSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identity', 'findingHistoryUpdateSource_identity' - The identity of the source that initiated the finding change event. For
-- example, the Amazon Resource Name (ARN) of a partner that calls
-- BatchImportFindings or of a customer that calls BatchUpdateFindings.
--
-- 'type'', 'findingHistoryUpdateSource_type' - Describes the type of finding change event, such as a call to
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
-- (by an integrated Amazon Web Service or third party partner integration)
-- or
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
-- (by a Security Hub customer).
newFindingHistoryUpdateSource ::
  FindingHistoryUpdateSource
newFindingHistoryUpdateSource =
  FindingHistoryUpdateSource'
    { identity =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The identity of the source that initiated the finding change event. For
-- example, the Amazon Resource Name (ARN) of a partner that calls
-- BatchImportFindings or of a customer that calls BatchUpdateFindings.
findingHistoryUpdateSource_identity :: Lens.Lens' FindingHistoryUpdateSource (Prelude.Maybe Prelude.Text)
findingHistoryUpdateSource_identity = Lens.lens (\FindingHistoryUpdateSource' {identity} -> identity) (\s@FindingHistoryUpdateSource' {} a -> s {identity = a} :: FindingHistoryUpdateSource)

-- | Describes the type of finding change event, such as a call to
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchImportFindings.html BatchImportFindings>
-- (by an integrated Amazon Web Service or third party partner integration)
-- or
-- <https://docs.aws.amazon.com/securityhub/1.0/APIReference/API_BatchUpdateFindings.html BatchUpdateFindings>
-- (by a Security Hub customer).
findingHistoryUpdateSource_type :: Lens.Lens' FindingHistoryUpdateSource (Prelude.Maybe FindingHistoryUpdateSourceType)
findingHistoryUpdateSource_type = Lens.lens (\FindingHistoryUpdateSource' {type'} -> type') (\s@FindingHistoryUpdateSource' {} a -> s {type' = a} :: FindingHistoryUpdateSource)

instance Data.FromJSON FindingHistoryUpdateSource where
  parseJSON =
    Data.withObject
      "FindingHistoryUpdateSource"
      ( \x ->
          FindingHistoryUpdateSource'
            Prelude.<$> (x Data..:? "Identity")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable FindingHistoryUpdateSource where
  hashWithSalt _salt FindingHistoryUpdateSource' {..} =
    _salt
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FindingHistoryUpdateSource where
  rnf FindingHistoryUpdateSource' {..} =
    Prelude.rnf identity
      `Prelude.seq` Prelude.rnf type'
