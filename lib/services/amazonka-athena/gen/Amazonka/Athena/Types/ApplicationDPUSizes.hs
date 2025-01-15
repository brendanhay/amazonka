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
-- Module      : Amazonka.Athena.Types.ApplicationDPUSizes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.ApplicationDPUSizes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the application runtime IDs and their supported DPU sizes.
--
-- /See:/ 'newApplicationDPUSizes' smart constructor.
data ApplicationDPUSizes = ApplicationDPUSizes'
  { -- | The name of the supported application runtime (for example,
    -- @Jupyter 1.0@).
    applicationRuntimeId :: Prelude.Maybe Prelude.Text,
    -- | A list of the supported DPU sizes that the application runtime supports.
    supportedDPUSizes :: Prelude.Maybe [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationDPUSizes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationRuntimeId', 'applicationDPUSizes_applicationRuntimeId' - The name of the supported application runtime (for example,
-- @Jupyter 1.0@).
--
-- 'supportedDPUSizes', 'applicationDPUSizes_supportedDPUSizes' - A list of the supported DPU sizes that the application runtime supports.
newApplicationDPUSizes ::
  ApplicationDPUSizes
newApplicationDPUSizes =
  ApplicationDPUSizes'
    { applicationRuntimeId =
        Prelude.Nothing,
      supportedDPUSizes = Prelude.Nothing
    }

-- | The name of the supported application runtime (for example,
-- @Jupyter 1.0@).
applicationDPUSizes_applicationRuntimeId :: Lens.Lens' ApplicationDPUSizes (Prelude.Maybe Prelude.Text)
applicationDPUSizes_applicationRuntimeId = Lens.lens (\ApplicationDPUSizes' {applicationRuntimeId} -> applicationRuntimeId) (\s@ApplicationDPUSizes' {} a -> s {applicationRuntimeId = a} :: ApplicationDPUSizes)

-- | A list of the supported DPU sizes that the application runtime supports.
applicationDPUSizes_supportedDPUSizes :: Lens.Lens' ApplicationDPUSizes (Prelude.Maybe [Prelude.Int])
applicationDPUSizes_supportedDPUSizes = Lens.lens (\ApplicationDPUSizes' {supportedDPUSizes} -> supportedDPUSizes) (\s@ApplicationDPUSizes' {} a -> s {supportedDPUSizes = a} :: ApplicationDPUSizes) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ApplicationDPUSizes where
  parseJSON =
    Data.withObject
      "ApplicationDPUSizes"
      ( \x ->
          ApplicationDPUSizes'
            Prelude.<$> (x Data..:? "ApplicationRuntimeId")
            Prelude.<*> ( x
                            Data..:? "SupportedDPUSizes"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ApplicationDPUSizes where
  hashWithSalt _salt ApplicationDPUSizes' {..} =
    _salt
      `Prelude.hashWithSalt` applicationRuntimeId
      `Prelude.hashWithSalt` supportedDPUSizes

instance Prelude.NFData ApplicationDPUSizes where
  rnf ApplicationDPUSizes' {..} =
    Prelude.rnf applicationRuntimeId `Prelude.seq`
      Prelude.rnf supportedDPUSizes
