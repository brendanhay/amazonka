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
-- Module      : Amazonka.APIGateway.Types.BasePathMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.BasePathMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the base path that callers of the API must provide as part of
-- the URL after the domain name.
--
-- /See:/ 'newBasePathMapping' smart constructor.
data BasePathMapping = BasePathMapping'
  { -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Maybe Prelude.Text,
    -- | The name of the associated stage.
    stage :: Prelude.Maybe Prelude.Text,
    -- | The base path name that callers of the API must provide as part of the
    -- URL after the domain name.
    basePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BasePathMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'basePathMapping_restApiId' - The string identifier of the associated RestApi.
--
-- 'stage', 'basePathMapping_stage' - The name of the associated stage.
--
-- 'basePath', 'basePathMapping_basePath' - The base path name that callers of the API must provide as part of the
-- URL after the domain name.
newBasePathMapping ::
  BasePathMapping
newBasePathMapping =
  BasePathMapping'
    { restApiId = Prelude.Nothing,
      stage = Prelude.Nothing,
      basePath = Prelude.Nothing
    }

-- | The string identifier of the associated RestApi.
basePathMapping_restApiId :: Lens.Lens' BasePathMapping (Prelude.Maybe Prelude.Text)
basePathMapping_restApiId = Lens.lens (\BasePathMapping' {restApiId} -> restApiId) (\s@BasePathMapping' {} a -> s {restApiId = a} :: BasePathMapping)

-- | The name of the associated stage.
basePathMapping_stage :: Lens.Lens' BasePathMapping (Prelude.Maybe Prelude.Text)
basePathMapping_stage = Lens.lens (\BasePathMapping' {stage} -> stage) (\s@BasePathMapping' {} a -> s {stage = a} :: BasePathMapping)

-- | The base path name that callers of the API must provide as part of the
-- URL after the domain name.
basePathMapping_basePath :: Lens.Lens' BasePathMapping (Prelude.Maybe Prelude.Text)
basePathMapping_basePath = Lens.lens (\BasePathMapping' {basePath} -> basePath) (\s@BasePathMapping' {} a -> s {basePath = a} :: BasePathMapping)

instance Data.FromJSON BasePathMapping where
  parseJSON =
    Data.withObject
      "BasePathMapping"
      ( \x ->
          BasePathMapping'
            Prelude.<$> (x Data..:? "restApiId")
            Prelude.<*> (x Data..:? "stage")
            Prelude.<*> (x Data..:? "basePath")
      )

instance Prelude.Hashable BasePathMapping where
  hashWithSalt _salt BasePathMapping' {..} =
    _salt `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` basePath

instance Prelude.NFData BasePathMapping where
  rnf BasePathMapping' {..} =
    Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf basePath
