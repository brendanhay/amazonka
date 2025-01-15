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
-- Module      : Amazonka.LakeFormation.Types.DataLocationResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DataLocationResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure for a data location object where permissions are granted or
-- revoked.
--
-- /See:/ 'newDataLocationResource' smart constructor.
data DataLocationResource = DataLocationResource'
  { -- | The identifier for the Data Catalog where the location is registered
    -- with Lake Formation. By default, it is the account ID of the caller.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the data
    -- location resource.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLocationResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'dataLocationResource_catalogId' - The identifier for the Data Catalog where the location is registered
-- with Lake Formation. By default, it is the account ID of the caller.
--
-- 'resourceArn', 'dataLocationResource_resourceArn' - The Amazon Resource Name (ARN) that uniquely identifies the data
-- location resource.
newDataLocationResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  DataLocationResource
newDataLocationResource pResourceArn_ =
  DataLocationResource'
    { catalogId = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The identifier for the Data Catalog where the location is registered
-- with Lake Formation. By default, it is the account ID of the caller.
dataLocationResource_catalogId :: Lens.Lens' DataLocationResource (Prelude.Maybe Prelude.Text)
dataLocationResource_catalogId = Lens.lens (\DataLocationResource' {catalogId} -> catalogId) (\s@DataLocationResource' {} a -> s {catalogId = a} :: DataLocationResource)

-- | The Amazon Resource Name (ARN) that uniquely identifies the data
-- location resource.
dataLocationResource_resourceArn :: Lens.Lens' DataLocationResource Prelude.Text
dataLocationResource_resourceArn = Lens.lens (\DataLocationResource' {resourceArn} -> resourceArn) (\s@DataLocationResource' {} a -> s {resourceArn = a} :: DataLocationResource)

instance Data.FromJSON DataLocationResource where
  parseJSON =
    Data.withObject
      "DataLocationResource"
      ( \x ->
          DataLocationResource'
            Prelude.<$> (x Data..:? "CatalogId")
            Prelude.<*> (x Data..: "ResourceArn")
      )

instance Prelude.Hashable DataLocationResource where
  hashWithSalt _salt DataLocationResource' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DataLocationResource where
  rnf DataLocationResource' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf resourceArn

instance Data.ToJSON DataLocationResource where
  toJSON DataLocationResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )
