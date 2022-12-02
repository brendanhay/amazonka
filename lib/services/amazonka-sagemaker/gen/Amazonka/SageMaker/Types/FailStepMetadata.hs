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
-- Module      : Amazonka.SageMaker.Types.FailStepMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FailStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The container for the metadata for Fail step.
--
-- /See:/ 'newFailStepMetadata' smart constructor.
data FailStepMetadata = FailStepMetadata'
  { -- | A message that you define and then is processed and rendered by the Fail
    -- step when the error occurs.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorMessage', 'failStepMetadata_errorMessage' - A message that you define and then is processed and rendered by the Fail
-- step when the error occurs.
newFailStepMetadata ::
  FailStepMetadata
newFailStepMetadata =
  FailStepMetadata' {errorMessage = Prelude.Nothing}

-- | A message that you define and then is processed and rendered by the Fail
-- step when the error occurs.
failStepMetadata_errorMessage :: Lens.Lens' FailStepMetadata (Prelude.Maybe Prelude.Text)
failStepMetadata_errorMessage = Lens.lens (\FailStepMetadata' {errorMessage} -> errorMessage) (\s@FailStepMetadata' {} a -> s {errorMessage = a} :: FailStepMetadata)

instance Data.FromJSON FailStepMetadata where
  parseJSON =
    Data.withObject
      "FailStepMetadata"
      ( \x ->
          FailStepMetadata'
            Prelude.<$> (x Data..:? "ErrorMessage")
      )

instance Prelude.Hashable FailStepMetadata where
  hashWithSalt _salt FailStepMetadata' {..} =
    _salt `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData FailStepMetadata where
  rnf FailStepMetadata' {..} = Prelude.rnf errorMessage
