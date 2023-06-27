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
-- Module      : Amazonka.AppFlow.Types.PardotSourceProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.PardotSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Salesforce Pardot is being used as
-- a source.
--
-- /See:/ 'newPardotSourceProperties' smart constructor.
data PardotSourceProperties = PardotSourceProperties'
  { -- | The object specified in the Salesforce Pardot flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PardotSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'pardotSourceProperties_object' - The object specified in the Salesforce Pardot flow source.
newPardotSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  PardotSourceProperties
newPardotSourceProperties pObject_ =
  PardotSourceProperties' {object' = pObject_}

-- | The object specified in the Salesforce Pardot flow source.
pardotSourceProperties_object :: Lens.Lens' PardotSourceProperties Prelude.Text
pardotSourceProperties_object = Lens.lens (\PardotSourceProperties' {object'} -> object') (\s@PardotSourceProperties' {} a -> s {object' = a} :: PardotSourceProperties)

instance Data.FromJSON PardotSourceProperties where
  parseJSON =
    Data.withObject
      "PardotSourceProperties"
      ( \x ->
          PardotSourceProperties'
            Prelude.<$> (x Data..: "object")
      )

instance Prelude.Hashable PardotSourceProperties where
  hashWithSalt _salt PardotSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData PardotSourceProperties where
  rnf PardotSourceProperties' {..} = Prelude.rnf object'

instance Data.ToJSON PardotSourceProperties where
  toJSON PardotSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Data..= object')]
      )
