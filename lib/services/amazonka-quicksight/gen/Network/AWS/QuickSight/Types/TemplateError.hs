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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TemplateErrorType

-- | List of errors that occurred when the template version creation failed.
--
-- /See:/ 'newTemplateError' smart constructor.
data TemplateError = TemplateError'
  { -- | Type of error.
    type' :: Prelude.Maybe TemplateErrorType,
    -- | Description of the error type.
    message :: Prelude.Maybe Prelude.Text
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
-- 'type'', 'templateError_type' - Type of error.
--
-- 'message', 'templateError_message' - Description of the error type.
newTemplateError ::
  TemplateError
newTemplateError =
  TemplateError'
    { type' = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | Type of error.
templateError_type :: Lens.Lens' TemplateError (Prelude.Maybe TemplateErrorType)
templateError_type = Lens.lens (\TemplateError' {type'} -> type') (\s@TemplateError' {} a -> s {type' = a} :: TemplateError)

-- | Description of the error type.
templateError_message :: Lens.Lens' TemplateError (Prelude.Maybe Prelude.Text)
templateError_message = Lens.lens (\TemplateError' {message} -> message) (\s@TemplateError' {} a -> s {message = a} :: TemplateError)

instance Core.FromJSON TemplateError where
  parseJSON =
    Core.withObject
      "TemplateError"
      ( \x ->
          TemplateError'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable TemplateError

instance Prelude.NFData TemplateError
