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
-- Module      : Amazonka.AppFlow.Types.HoneycodeDestinationProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.HoneycodeDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Amazon Honeycode is used as a
-- destination.
--
-- /See:/ 'newHoneycodeDestinationProperties' smart constructor.
data HoneycodeDestinationProperties = HoneycodeDestinationProperties'
  { errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    -- | The object specified in the Amazon Honeycode flow destination.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoneycodeDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorHandlingConfig', 'honeycodeDestinationProperties_errorHandlingConfig' - Undocumented member.
--
-- 'object'', 'honeycodeDestinationProperties_object' - The object specified in the Amazon Honeycode flow destination.
newHoneycodeDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  HoneycodeDestinationProperties
newHoneycodeDestinationProperties pObject_ =
  HoneycodeDestinationProperties'
    { errorHandlingConfig =
        Prelude.Nothing,
      object' = pObject_
    }

-- | Undocumented member.
honeycodeDestinationProperties_errorHandlingConfig :: Lens.Lens' HoneycodeDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
honeycodeDestinationProperties_errorHandlingConfig = Lens.lens (\HoneycodeDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@HoneycodeDestinationProperties' {} a -> s {errorHandlingConfig = a} :: HoneycodeDestinationProperties)

-- | The object specified in the Amazon Honeycode flow destination.
honeycodeDestinationProperties_object :: Lens.Lens' HoneycodeDestinationProperties Prelude.Text
honeycodeDestinationProperties_object = Lens.lens (\HoneycodeDestinationProperties' {object'} -> object') (\s@HoneycodeDestinationProperties' {} a -> s {object' = a} :: HoneycodeDestinationProperties)

instance Data.FromJSON HoneycodeDestinationProperties where
  parseJSON =
    Data.withObject
      "HoneycodeDestinationProperties"
      ( \x ->
          HoneycodeDestinationProperties'
            Prelude.<$> (x Data..:? "errorHandlingConfig")
            Prelude.<*> (x Data..: "object")
      )

instance
  Prelude.Hashable
    HoneycodeDestinationProperties
  where
  hashWithSalt
    _salt
    HoneycodeDestinationProperties' {..} =
      _salt `Prelude.hashWithSalt` errorHandlingConfig
        `Prelude.hashWithSalt` object'

instance
  Prelude.NFData
    HoneycodeDestinationProperties
  where
  rnf HoneycodeDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig
      `Prelude.seq` Prelude.rnf object'

instance Data.ToJSON HoneycodeDestinationProperties where
  toJSON HoneycodeDestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Data..=)
              Prelude.<$> errorHandlingConfig,
            Prelude.Just ("object" Data..= object')
          ]
      )
