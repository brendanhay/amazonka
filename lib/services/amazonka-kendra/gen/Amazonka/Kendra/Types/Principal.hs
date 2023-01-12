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
-- Module      : Amazonka.Kendra.Types.Principal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.Principal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.PrincipalType
import Amazonka.Kendra.Types.ReadAccessType
import qualified Amazonka.Prelude as Prelude

-- | Provides user and group information for
-- <https://docs.aws.amazon.com/kendra/latest/dg/user-context-filter.html user context filtering>.
--
-- /See:/ 'newPrincipal' smart constructor.
data Principal = Principal'
  { -- | The identifier of the data source the principal should access documents
    -- from.
    dataSourceId :: Prelude.Maybe Prelude.Text,
    -- | The name of the user or group.
    name :: Prelude.Text,
    -- | The type of principal.
    type' :: PrincipalType,
    -- | Whether to allow or deny document access to the principal.
    access :: ReadAccessType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Principal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceId', 'principal_dataSourceId' - The identifier of the data source the principal should access documents
-- from.
--
-- 'name', 'principal_name' - The name of the user or group.
--
-- 'type'', 'principal_type' - The type of principal.
--
-- 'access', 'principal_access' - Whether to allow or deny document access to the principal.
newPrincipal ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  PrincipalType ->
  -- | 'access'
  ReadAccessType ->
  Principal
newPrincipal pName_ pType_ pAccess_ =
  Principal'
    { dataSourceId = Prelude.Nothing,
      name = pName_,
      type' = pType_,
      access = pAccess_
    }

-- | The identifier of the data source the principal should access documents
-- from.
principal_dataSourceId :: Lens.Lens' Principal (Prelude.Maybe Prelude.Text)
principal_dataSourceId = Lens.lens (\Principal' {dataSourceId} -> dataSourceId) (\s@Principal' {} a -> s {dataSourceId = a} :: Principal)

-- | The name of the user or group.
principal_name :: Lens.Lens' Principal Prelude.Text
principal_name = Lens.lens (\Principal' {name} -> name) (\s@Principal' {} a -> s {name = a} :: Principal)

-- | The type of principal.
principal_type :: Lens.Lens' Principal PrincipalType
principal_type = Lens.lens (\Principal' {type'} -> type') (\s@Principal' {} a -> s {type' = a} :: Principal)

-- | Whether to allow or deny document access to the principal.
principal_access :: Lens.Lens' Principal ReadAccessType
principal_access = Lens.lens (\Principal' {access} -> access) (\s@Principal' {} a -> s {access = a} :: Principal)

instance Data.FromJSON Principal where
  parseJSON =
    Data.withObject
      "Principal"
      ( \x ->
          Principal'
            Prelude.<$> (x Data..:? "DataSourceId")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "Access")
      )

instance Prelude.Hashable Principal where
  hashWithSalt _salt Principal' {..} =
    _salt `Prelude.hashWithSalt` dataSourceId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` access

instance Prelude.NFData Principal where
  rnf Principal' {..} =
    Prelude.rnf dataSourceId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf access

instance Data.ToJSON Principal where
  toJSON Principal' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSourceId" Data..=) Prelude.<$> dataSourceId,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Access" Data..= access)
          ]
      )
