{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The error code and description resulting from an operation.
--
-- /See:/ 'newRecordError' smart constructor.
data RecordError = RecordError'
  { -- | The numeric value of the error.
    code :: Prelude.Maybe Prelude.Text,
    -- | The description of the error.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { code = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The numeric value of the error.
recordError_code :: Lens.Lens' RecordError (Prelude.Maybe Prelude.Text)
recordError_code = Lens.lens (\RecordError' {code} -> code) (\s@RecordError' {} a -> s {code = a} :: RecordError)

-- | The description of the error.
recordError_description :: Lens.Lens' RecordError (Prelude.Maybe Prelude.Text)
recordError_description = Lens.lens (\RecordError' {description} -> description) (\s@RecordError' {} a -> s {description = a} :: RecordError)

instance Prelude.FromJSON RecordError where
  parseJSON =
    Prelude.withObject
      "RecordError"
      ( \x ->
          RecordError'
            Prelude.<$> (x Prelude..:? "Code")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable RecordError

instance Prelude.NFData RecordError
