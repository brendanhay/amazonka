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
-- Module      : Amazonka.Glue.Types.GetConnectionsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GetConnectionsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ConnectionType
import qualified Amazonka.Prelude as Prelude

-- | Filters the connection definitions that are returned by the
-- @GetConnections@ API operation.
--
-- /See:/ 'newGetConnectionsFilter' smart constructor.
data GetConnectionsFilter = GetConnectionsFilter'
  { -- | The type of connections to return. Currently, SFTP is not supported.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | A criteria string that must match the criteria recorded in the
    -- connection definition for that connection definition to be returned.
    matchCriteria :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectionsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionType', 'getConnectionsFilter_connectionType' - The type of connections to return. Currently, SFTP is not supported.
--
-- 'matchCriteria', 'getConnectionsFilter_matchCriteria' - A criteria string that must match the criteria recorded in the
-- connection definition for that connection definition to be returned.
newGetConnectionsFilter ::
  GetConnectionsFilter
newGetConnectionsFilter =
  GetConnectionsFilter'
    { connectionType =
        Prelude.Nothing,
      matchCriteria = Prelude.Nothing
    }

-- | The type of connections to return. Currently, SFTP is not supported.
getConnectionsFilter_connectionType :: Lens.Lens' GetConnectionsFilter (Prelude.Maybe ConnectionType)
getConnectionsFilter_connectionType = Lens.lens (\GetConnectionsFilter' {connectionType} -> connectionType) (\s@GetConnectionsFilter' {} a -> s {connectionType = a} :: GetConnectionsFilter)

-- | A criteria string that must match the criteria recorded in the
-- connection definition for that connection definition to be returned.
getConnectionsFilter_matchCriteria :: Lens.Lens' GetConnectionsFilter (Prelude.Maybe [Prelude.Text])
getConnectionsFilter_matchCriteria = Lens.lens (\GetConnectionsFilter' {matchCriteria} -> matchCriteria) (\s@GetConnectionsFilter' {} a -> s {matchCriteria = a} :: GetConnectionsFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable GetConnectionsFilter where
  hashWithSalt _salt GetConnectionsFilter' {..} =
    _salt
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` matchCriteria

instance Prelude.NFData GetConnectionsFilter where
  rnf GetConnectionsFilter' {..} =
    Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf matchCriteria

instance Data.ToJSON GetConnectionsFilter where
  toJSON GetConnectionsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConnectionType" Data..=)
              Prelude.<$> connectionType,
            ("MatchCriteria" Data..=) Prelude.<$> matchCriteria
          ]
      )
