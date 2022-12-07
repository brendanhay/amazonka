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
-- Module      : Amazonka.Forecast.Types.ExplainabilityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.ExplainabilityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the Explainability resource.
--
-- /See:/ 'newExplainabilityInfo' smart constructor.
data ExplainabilityInfo = ExplainabilityInfo'
  { -- | The status of the Explainability. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Explainability.
    explainabilityArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplainabilityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'explainabilityInfo_status' - The status of the Explainability. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- 'explainabilityArn', 'explainabilityInfo_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability.
newExplainabilityInfo ::
  ExplainabilityInfo
newExplainabilityInfo =
  ExplainabilityInfo'
    { status = Prelude.Nothing,
      explainabilityArn = Prelude.Nothing
    }

-- | The status of the Explainability. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
explainabilityInfo_status :: Lens.Lens' ExplainabilityInfo (Prelude.Maybe Prelude.Text)
explainabilityInfo_status = Lens.lens (\ExplainabilityInfo' {status} -> status) (\s@ExplainabilityInfo' {} a -> s {status = a} :: ExplainabilityInfo)

-- | The Amazon Resource Name (ARN) of the Explainability.
explainabilityInfo_explainabilityArn :: Lens.Lens' ExplainabilityInfo (Prelude.Maybe Prelude.Text)
explainabilityInfo_explainabilityArn = Lens.lens (\ExplainabilityInfo' {explainabilityArn} -> explainabilityArn) (\s@ExplainabilityInfo' {} a -> s {explainabilityArn = a} :: ExplainabilityInfo)

instance Data.FromJSON ExplainabilityInfo where
  parseJSON =
    Data.withObject
      "ExplainabilityInfo"
      ( \x ->
          ExplainabilityInfo'
            Prelude.<$> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ExplainabilityArn")
      )

instance Prelude.Hashable ExplainabilityInfo where
  hashWithSalt _salt ExplainabilityInfo' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` explainabilityArn

instance Prelude.NFData ExplainabilityInfo where
  rnf ExplainabilityInfo' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf explainabilityArn
