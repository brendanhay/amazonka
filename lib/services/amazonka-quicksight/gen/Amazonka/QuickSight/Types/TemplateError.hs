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
-- Module      : Amazonka.QuickSight.Types.TemplateError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TemplateErrorType

-- | List of errors that occurred when the template version creation failed.
--
-- /See:/ 'newTemplateError' smart constructor.
data TemplateError = TemplateError'
  { -- | Description of the error type.
    message :: Prelude.Maybe Prelude.Text,
    -- | Type of error.
    type' :: Prelude.Maybe TemplateErrorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'templateError_message' - Description of the error type.
--
-- 'type'', 'templateError_type' - Type of error.
newTemplateError ::
  TemplateError
newTemplateError =
  TemplateError'
    { message = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Description of the error type.
templateError_message :: Lens.Lens' TemplateError (Prelude.Maybe Prelude.Text)
templateError_message = Lens.lens (\TemplateError' {message} -> message) (\s@TemplateError' {} a -> s {message = a} :: TemplateError)

-- | Type of error.
templateError_type :: Lens.Lens' TemplateError (Prelude.Maybe TemplateErrorType)
templateError_type = Lens.lens (\TemplateError' {type'} -> type') (\s@TemplateError' {} a -> s {type' = a} :: TemplateError)

instance Data.FromJSON TemplateError where
  parseJSON =
    Data.withObject
      "TemplateError"
      ( \x ->
          TemplateError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable TemplateError where
  hashWithSalt _salt TemplateError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` type'

instance Prelude.NFData TemplateError where
  rnf TemplateError' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf type'
