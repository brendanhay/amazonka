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
-- Module      : Amazonka.AppFlow.Types.SAPODataSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SAPODataSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when using SAPOData as a flow source.
--
-- /See:/ 'newSAPODataSourceProperties' smart constructor.
data SAPODataSourceProperties = SAPODataSourceProperties'
  { -- | The object path specified in the SAPOData flow source.
    objectPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SAPODataSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectPath', 'sAPODataSourceProperties_objectPath' - The object path specified in the SAPOData flow source.
newSAPODataSourceProperties ::
  SAPODataSourceProperties
newSAPODataSourceProperties =
  SAPODataSourceProperties'
    { objectPath =
        Prelude.Nothing
    }

-- | The object path specified in the SAPOData flow source.
sAPODataSourceProperties_objectPath :: Lens.Lens' SAPODataSourceProperties (Prelude.Maybe Prelude.Text)
sAPODataSourceProperties_objectPath = Lens.lens (\SAPODataSourceProperties' {objectPath} -> objectPath) (\s@SAPODataSourceProperties' {} a -> s {objectPath = a} :: SAPODataSourceProperties)

instance Data.FromJSON SAPODataSourceProperties where
  parseJSON =
    Data.withObject
      "SAPODataSourceProperties"
      ( \x ->
          SAPODataSourceProperties'
            Prelude.<$> (x Data..:? "objectPath")
      )

instance Prelude.Hashable SAPODataSourceProperties where
  hashWithSalt _salt SAPODataSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` objectPath

instance Prelude.NFData SAPODataSourceProperties where
  rnf SAPODataSourceProperties' {..} =
    Prelude.rnf objectPath

instance Data.ToJSON SAPODataSourceProperties where
  toJSON SAPODataSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [("objectPath" Data..=) Prelude.<$> objectPath]
      )
