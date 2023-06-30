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
-- Module      : Amazonka.AppFlow.Types.MarketoDestinationProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.MarketoDestinationProperties where

import Amazonka.AppFlow.Types.ErrorHandlingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that Amazon AppFlow applies when you use Marketo as a
-- flow destination.
--
-- /See:/ 'newMarketoDestinationProperties' smart constructor.
data MarketoDestinationProperties = MarketoDestinationProperties'
  { errorHandlingConfig :: Prelude.Maybe ErrorHandlingConfig,
    -- | The object specified in the Marketo flow destination.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MarketoDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorHandlingConfig', 'marketoDestinationProperties_errorHandlingConfig' - Undocumented member.
--
-- 'object'', 'marketoDestinationProperties_object' - The object specified in the Marketo flow destination.
newMarketoDestinationProperties ::
  -- | 'object''
  Prelude.Text ->
  MarketoDestinationProperties
newMarketoDestinationProperties pObject_ =
  MarketoDestinationProperties'
    { errorHandlingConfig =
        Prelude.Nothing,
      object' = pObject_
    }

-- | Undocumented member.
marketoDestinationProperties_errorHandlingConfig :: Lens.Lens' MarketoDestinationProperties (Prelude.Maybe ErrorHandlingConfig)
marketoDestinationProperties_errorHandlingConfig = Lens.lens (\MarketoDestinationProperties' {errorHandlingConfig} -> errorHandlingConfig) (\s@MarketoDestinationProperties' {} a -> s {errorHandlingConfig = a} :: MarketoDestinationProperties)

-- | The object specified in the Marketo flow destination.
marketoDestinationProperties_object :: Lens.Lens' MarketoDestinationProperties Prelude.Text
marketoDestinationProperties_object = Lens.lens (\MarketoDestinationProperties' {object'} -> object') (\s@MarketoDestinationProperties' {} a -> s {object' = a} :: MarketoDestinationProperties)

instance Data.FromJSON MarketoDestinationProperties where
  parseJSON =
    Data.withObject
      "MarketoDestinationProperties"
      ( \x ->
          MarketoDestinationProperties'
            Prelude.<$> (x Data..:? "errorHandlingConfig")
            Prelude.<*> (x Data..: "object")
      )

instance
  Prelude.Hashable
    MarketoDestinationProperties
  where
  hashWithSalt _salt MarketoDestinationProperties' {..} =
    _salt
      `Prelude.hashWithSalt` errorHandlingConfig
      `Prelude.hashWithSalt` object'

instance Prelude.NFData MarketoDestinationProperties where
  rnf MarketoDestinationProperties' {..} =
    Prelude.rnf errorHandlingConfig
      `Prelude.seq` Prelude.rnf object'

instance Data.ToJSON MarketoDestinationProperties where
  toJSON MarketoDestinationProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("errorHandlingConfig" Data..=)
              Prelude.<$> errorHandlingConfig,
            Prelude.Just ("object" Data..= object')
          ]
      )
