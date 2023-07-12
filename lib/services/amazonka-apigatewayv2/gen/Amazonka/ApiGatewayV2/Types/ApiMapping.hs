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
-- Module      : Amazonka.ApiGatewayV2.Types.ApiMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.ApiMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an API mapping.
--
-- /See:/ 'newApiMapping' smart constructor.
data ApiMapping = ApiMapping'
  { -- | The API mapping identifier.
    apiMappingId :: Prelude.Maybe Prelude.Text,
    -- | The API mapping key.
    apiMappingKey :: Prelude.Maybe Prelude.Text,
    -- | The API stage.
    stage :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiMappingId', 'apiMapping_apiMappingId' - The API mapping identifier.
--
-- 'apiMappingKey', 'apiMapping_apiMappingKey' - The API mapping key.
--
-- 'stage', 'apiMapping_stage' - The API stage.
--
-- 'apiId', 'apiMapping_apiId' - The API identifier.
newApiMapping ::
  -- | 'stage'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  ApiMapping
newApiMapping pStage_ pApiId_ =
  ApiMapping'
    { apiMappingId = Prelude.Nothing,
      apiMappingKey = Prelude.Nothing,
      stage = pStage_,
      apiId = pApiId_
    }

-- | The API mapping identifier.
apiMapping_apiMappingId :: Lens.Lens' ApiMapping (Prelude.Maybe Prelude.Text)
apiMapping_apiMappingId = Lens.lens (\ApiMapping' {apiMappingId} -> apiMappingId) (\s@ApiMapping' {} a -> s {apiMappingId = a} :: ApiMapping)

-- | The API mapping key.
apiMapping_apiMappingKey :: Lens.Lens' ApiMapping (Prelude.Maybe Prelude.Text)
apiMapping_apiMappingKey = Lens.lens (\ApiMapping' {apiMappingKey} -> apiMappingKey) (\s@ApiMapping' {} a -> s {apiMappingKey = a} :: ApiMapping)

-- | The API stage.
apiMapping_stage :: Lens.Lens' ApiMapping Prelude.Text
apiMapping_stage = Lens.lens (\ApiMapping' {stage} -> stage) (\s@ApiMapping' {} a -> s {stage = a} :: ApiMapping)

-- | The API identifier.
apiMapping_apiId :: Lens.Lens' ApiMapping Prelude.Text
apiMapping_apiId = Lens.lens (\ApiMapping' {apiId} -> apiId) (\s@ApiMapping' {} a -> s {apiId = a} :: ApiMapping)

instance Data.FromJSON ApiMapping where
  parseJSON =
    Data.withObject
      "ApiMapping"
      ( \x ->
          ApiMapping'
            Prelude.<$> (x Data..:? "apiMappingId")
            Prelude.<*> (x Data..:? "apiMappingKey")
            Prelude.<*> (x Data..: "stage")
            Prelude.<*> (x Data..: "apiId")
      )

instance Prelude.Hashable ApiMapping where
  hashWithSalt _salt ApiMapping' {..} =
    _salt
      `Prelude.hashWithSalt` apiMappingId
      `Prelude.hashWithSalt` apiMappingKey
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData ApiMapping where
  rnf ApiMapping' {..} =
    Prelude.rnf apiMappingId
      `Prelude.seq` Prelude.rnf apiMappingKey
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf apiId
