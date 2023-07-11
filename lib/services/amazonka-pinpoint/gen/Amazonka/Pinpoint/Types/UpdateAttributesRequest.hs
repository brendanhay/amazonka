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
-- Module      : Amazonka.Pinpoint.Types.UpdateAttributesRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.UpdateAttributesRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies one or more attributes to remove from all the endpoints that
-- are associated with an application.
--
-- /See:/ 'newUpdateAttributesRequest' smart constructor.
data UpdateAttributesRequest = UpdateAttributesRequest'
  { -- | An array of the attributes to remove from all the endpoints that are
    -- associated with the application. The array can specify the complete,
    -- exact name of each attribute to remove or it can specify a glob pattern
    -- that an attribute name must match in order for the attribute to be
    -- removed.
    blacklist :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAttributesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blacklist', 'updateAttributesRequest_blacklist' - An array of the attributes to remove from all the endpoints that are
-- associated with the application. The array can specify the complete,
-- exact name of each attribute to remove or it can specify a glob pattern
-- that an attribute name must match in order for the attribute to be
-- removed.
newUpdateAttributesRequest ::
  UpdateAttributesRequest
newUpdateAttributesRequest =
  UpdateAttributesRequest'
    { blacklist =
        Prelude.Nothing
    }

-- | An array of the attributes to remove from all the endpoints that are
-- associated with the application. The array can specify the complete,
-- exact name of each attribute to remove or it can specify a glob pattern
-- that an attribute name must match in order for the attribute to be
-- removed.
updateAttributesRequest_blacklist :: Lens.Lens' UpdateAttributesRequest (Prelude.Maybe [Prelude.Text])
updateAttributesRequest_blacklist = Lens.lens (\UpdateAttributesRequest' {blacklist} -> blacklist) (\s@UpdateAttributesRequest' {} a -> s {blacklist = a} :: UpdateAttributesRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UpdateAttributesRequest where
  hashWithSalt _salt UpdateAttributesRequest' {..} =
    _salt `Prelude.hashWithSalt` blacklist

instance Prelude.NFData UpdateAttributesRequest where
  rnf UpdateAttributesRequest' {..} =
    Prelude.rnf blacklist

instance Data.ToJSON UpdateAttributesRequest where
  toJSON UpdateAttributesRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Blacklist" Data..=) Prelude.<$> blacklist]
      )
