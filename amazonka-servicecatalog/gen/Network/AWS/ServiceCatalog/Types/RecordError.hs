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
-- Module      : Network.AWS.ServiceCatalog.Types.RecordError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordError where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The error code and description resulting from an operation.
--
-- /See:/ 'newRecordError' smart constructor.
data RecordError = RecordError'
  { -- | The numeric value of the error.
    code :: Core.Maybe Core.Text,
    -- | The description of the error.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecordError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'recordError_code' - The numeric value of the error.
--
-- 'description', 'recordError_description' - The description of the error.
newRecordError ::
  RecordError
newRecordError =
  RecordError'
    { code = Core.Nothing,
      description = Core.Nothing
    }

-- | The numeric value of the error.
recordError_code :: Lens.Lens' RecordError (Core.Maybe Core.Text)
recordError_code = Lens.lens (\RecordError' {code} -> code) (\s@RecordError' {} a -> s {code = a} :: RecordError)

-- | The description of the error.
recordError_description :: Lens.Lens' RecordError (Core.Maybe Core.Text)
recordError_description = Lens.lens (\RecordError' {description} -> description) (\s@RecordError' {} a -> s {description = a} :: RecordError)

instance Core.FromJSON RecordError where
  parseJSON =
    Core.withObject
      "RecordError"
      ( \x ->
          RecordError'
            Core.<$> (x Core..:? "Code")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable RecordError

instance Core.NFData RecordError
