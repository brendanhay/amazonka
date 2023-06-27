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
-- Module      : Amazonka.OsIs.Types.ValidationMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.ValidationMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A validation message associated with a @ValidatePipeline@ request in
-- OpenSearch Ingestion.
--
-- /See:/ 'newValidationMessage' smart constructor.
data ValidationMessage = ValidationMessage'
  { -- | The validation message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValidationMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'validationMessage_message' - The validation message.
newValidationMessage ::
  ValidationMessage
newValidationMessage =
  ValidationMessage' {message = Prelude.Nothing}

-- | The validation message.
validationMessage_message :: Lens.Lens' ValidationMessage (Prelude.Maybe Prelude.Text)
validationMessage_message = Lens.lens (\ValidationMessage' {message} -> message) (\s@ValidationMessage' {} a -> s {message = a} :: ValidationMessage)

instance Data.FromJSON ValidationMessage where
  parseJSON =
    Data.withObject
      "ValidationMessage"
      ( \x ->
          ValidationMessage'
            Prelude.<$> (x Data..:? "Message")
      )

instance Prelude.Hashable ValidationMessage where
  hashWithSalt _salt ValidationMessage' {..} =
    _salt `Prelude.hashWithSalt` message

instance Prelude.NFData ValidationMessage where
  rnf ValidationMessage' {..} = Prelude.rnf message
