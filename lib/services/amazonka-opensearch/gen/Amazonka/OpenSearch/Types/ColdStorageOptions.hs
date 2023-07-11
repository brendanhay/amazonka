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
-- Module      : Amazonka.OpenSearch.Types.ColdStorageOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ColdStorageOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Container for the parameters required to enable cold storage for an
-- OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cold-storage.html Cold storage for Amazon OpenSearch Service>.
--
-- /See:/ 'newColdStorageOptions' smart constructor.
data ColdStorageOptions = ColdStorageOptions'
  { -- | Whether to enable or disable cold storage on the domain.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColdStorageOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'coldStorageOptions_enabled' - Whether to enable or disable cold storage on the domain.
newColdStorageOptions ::
  -- | 'enabled'
  Prelude.Bool ->
  ColdStorageOptions
newColdStorageOptions pEnabled_ =
  ColdStorageOptions' {enabled = pEnabled_}

-- | Whether to enable or disable cold storage on the domain.
coldStorageOptions_enabled :: Lens.Lens' ColdStorageOptions Prelude.Bool
coldStorageOptions_enabled = Lens.lens (\ColdStorageOptions' {enabled} -> enabled) (\s@ColdStorageOptions' {} a -> s {enabled = a} :: ColdStorageOptions)

instance Data.FromJSON ColdStorageOptions where
  parseJSON =
    Data.withObject
      "ColdStorageOptions"
      ( \x ->
          ColdStorageOptions'
            Prelude.<$> (x Data..: "Enabled")
      )

instance Prelude.Hashable ColdStorageOptions where
  hashWithSalt _salt ColdStorageOptions' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData ColdStorageOptions where
  rnf ColdStorageOptions' {..} = Prelude.rnf enabled

instance Data.ToJSON ColdStorageOptions where
  toJSON ColdStorageOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Enabled" Data..= enabled)]
      )
