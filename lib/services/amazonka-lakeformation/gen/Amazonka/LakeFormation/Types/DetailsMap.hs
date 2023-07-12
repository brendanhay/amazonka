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
-- Module      : Amazonka.LakeFormation.Types.DetailsMap
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.DetailsMap where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure containing the additional details to be returned in the
-- @AdditionalDetails@ attribute of @PrincipalResourcePermissions@.
--
-- If a catalog resource is shared through Resource Access Manager (RAM),
-- then there will exist a corresponding RAM resource share ARN.
--
-- /See:/ 'newDetailsMap' smart constructor.
data DetailsMap = DetailsMap'
  { -- | A resource share ARN for a catalog resource shared through RAM.
    resourceShare :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetailsMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceShare', 'detailsMap_resourceShare' - A resource share ARN for a catalog resource shared through RAM.
newDetailsMap ::
  DetailsMap
newDetailsMap =
  DetailsMap' {resourceShare = Prelude.Nothing}

-- | A resource share ARN for a catalog resource shared through RAM.
detailsMap_resourceShare :: Lens.Lens' DetailsMap (Prelude.Maybe [Prelude.Text])
detailsMap_resourceShare = Lens.lens (\DetailsMap' {resourceShare} -> resourceShare) (\s@DetailsMap' {} a -> s {resourceShare = a} :: DetailsMap) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DetailsMap where
  parseJSON =
    Data.withObject
      "DetailsMap"
      ( \x ->
          DetailsMap'
            Prelude.<$> (x Data..:? "ResourceShare" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable DetailsMap where
  hashWithSalt _salt DetailsMap' {..} =
    _salt `Prelude.hashWithSalt` resourceShare

instance Prelude.NFData DetailsMap where
  rnf DetailsMap' {..} = Prelude.rnf resourceShare
