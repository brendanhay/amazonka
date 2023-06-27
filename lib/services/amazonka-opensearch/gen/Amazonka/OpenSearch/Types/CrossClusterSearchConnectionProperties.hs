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
-- Module      : Amazonka.OpenSearch.Types.CrossClusterSearchConnectionProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.CrossClusterSearchConnectionProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.SkipUnavailableStatus
import qualified Amazonka.Prelude as Prelude

-- | Cross cluster search specific connection properties.
--
-- /See:/ 'newCrossClusterSearchConnectionProperties' smart constructor.
data CrossClusterSearchConnectionProperties = CrossClusterSearchConnectionProperties'
  { -- | Status of SkipUnavailable param for outbound connection.
    skipUnavailable :: Prelude.Maybe SkipUnavailableStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossClusterSearchConnectionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skipUnavailable', 'crossClusterSearchConnectionProperties_skipUnavailable' - Status of SkipUnavailable param for outbound connection.
newCrossClusterSearchConnectionProperties ::
  CrossClusterSearchConnectionProperties
newCrossClusterSearchConnectionProperties =
  CrossClusterSearchConnectionProperties'
    { skipUnavailable =
        Prelude.Nothing
    }

-- | Status of SkipUnavailable param for outbound connection.
crossClusterSearchConnectionProperties_skipUnavailable :: Lens.Lens' CrossClusterSearchConnectionProperties (Prelude.Maybe SkipUnavailableStatus)
crossClusterSearchConnectionProperties_skipUnavailable = Lens.lens (\CrossClusterSearchConnectionProperties' {skipUnavailable} -> skipUnavailable) (\s@CrossClusterSearchConnectionProperties' {} a -> s {skipUnavailable = a} :: CrossClusterSearchConnectionProperties)

instance
  Data.FromJSON
    CrossClusterSearchConnectionProperties
  where
  parseJSON =
    Data.withObject
      "CrossClusterSearchConnectionProperties"
      ( \x ->
          CrossClusterSearchConnectionProperties'
            Prelude.<$> (x Data..:? "SkipUnavailable")
      )

instance
  Prelude.Hashable
    CrossClusterSearchConnectionProperties
  where
  hashWithSalt
    _salt
    CrossClusterSearchConnectionProperties' {..} =
      _salt `Prelude.hashWithSalt` skipUnavailable

instance
  Prelude.NFData
    CrossClusterSearchConnectionProperties
  where
  rnf CrossClusterSearchConnectionProperties' {..} =
    Prelude.rnf skipUnavailable

instance
  Data.ToJSON
    CrossClusterSearchConnectionProperties
  where
  toJSON CrossClusterSearchConnectionProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SkipUnavailable" Data..=)
              Prelude.<$> skipUnavailable
          ]
      )
