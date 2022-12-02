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
-- Module      : Amazonka.AppFlow.Types.InforNexusSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.InforNexusSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Infor Nexus is being used as a
-- source.
--
-- /See:/ 'newInforNexusSourceProperties' smart constructor.
data InforNexusSourceProperties = InforNexusSourceProperties'
  { -- | The object specified in the Infor Nexus flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InforNexusSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'inforNexusSourceProperties_object' - The object specified in the Infor Nexus flow source.
newInforNexusSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  InforNexusSourceProperties
newInforNexusSourceProperties pObject_ =
  InforNexusSourceProperties' {object' = pObject_}

-- | The object specified in the Infor Nexus flow source.
inforNexusSourceProperties_object :: Lens.Lens' InforNexusSourceProperties Prelude.Text
inforNexusSourceProperties_object = Lens.lens (\InforNexusSourceProperties' {object'} -> object') (\s@InforNexusSourceProperties' {} a -> s {object' = a} :: InforNexusSourceProperties)

instance Data.FromJSON InforNexusSourceProperties where
  parseJSON =
    Data.withObject
      "InforNexusSourceProperties"
      ( \x ->
          InforNexusSourceProperties'
            Prelude.<$> (x Data..: "object")
      )

instance Prelude.Hashable InforNexusSourceProperties where
  hashWithSalt _salt InforNexusSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData InforNexusSourceProperties where
  rnf InforNexusSourceProperties' {..} =
    Prelude.rnf object'

instance Data.ToJSON InforNexusSourceProperties where
  toJSON InforNexusSourceProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Data..= object')]
      )
