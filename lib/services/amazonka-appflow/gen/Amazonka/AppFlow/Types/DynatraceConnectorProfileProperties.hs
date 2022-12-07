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
-- Module      : Amazonka.AppFlow.Types.DynatraceConnectorProfileProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.DynatraceConnectorProfileProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile properties required by Dynatrace.
--
-- /See:/ 'newDynatraceConnectorProfileProperties' smart constructor.
data DynatraceConnectorProfileProperties = DynatraceConnectorProfileProperties'
  { -- | The location of the Dynatrace resource.
    instanceUrl :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DynatraceConnectorProfileProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceUrl', 'dynatraceConnectorProfileProperties_instanceUrl' - The location of the Dynatrace resource.
newDynatraceConnectorProfileProperties ::
  -- | 'instanceUrl'
  Prelude.Text ->
  DynatraceConnectorProfileProperties
newDynatraceConnectorProfileProperties pInstanceUrl_ =
  DynatraceConnectorProfileProperties'
    { instanceUrl =
        pInstanceUrl_
    }

-- | The location of the Dynatrace resource.
dynatraceConnectorProfileProperties_instanceUrl :: Lens.Lens' DynatraceConnectorProfileProperties Prelude.Text
dynatraceConnectorProfileProperties_instanceUrl = Lens.lens (\DynatraceConnectorProfileProperties' {instanceUrl} -> instanceUrl) (\s@DynatraceConnectorProfileProperties' {} a -> s {instanceUrl = a} :: DynatraceConnectorProfileProperties)

instance
  Data.FromJSON
    DynatraceConnectorProfileProperties
  where
  parseJSON =
    Data.withObject
      "DynatraceConnectorProfileProperties"
      ( \x ->
          DynatraceConnectorProfileProperties'
            Prelude.<$> (x Data..: "instanceUrl")
      )

instance
  Prelude.Hashable
    DynatraceConnectorProfileProperties
  where
  hashWithSalt
    _salt
    DynatraceConnectorProfileProperties' {..} =
      _salt `Prelude.hashWithSalt` instanceUrl

instance
  Prelude.NFData
    DynatraceConnectorProfileProperties
  where
  rnf DynatraceConnectorProfileProperties' {..} =
    Prelude.rnf instanceUrl

instance
  Data.ToJSON
    DynatraceConnectorProfileProperties
  where
  toJSON DynatraceConnectorProfileProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("instanceUrl" Data..= instanceUrl)]
      )
