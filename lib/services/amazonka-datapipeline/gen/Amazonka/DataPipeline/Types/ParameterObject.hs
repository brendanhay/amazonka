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
-- Module      : Amazonka.DataPipeline.Types.ParameterObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types.ParameterObject where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataPipeline.Types.ParameterAttribute
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a parameter object.
--
-- /See:/ 'newParameterObject' smart constructor.
data ParameterObject = ParameterObject'
  { -- | The ID of the parameter object.
    id :: Prelude.Text,
    -- | The attributes of the parameter object.
    attributes :: [ParameterAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'parameterObject_id' - The ID of the parameter object.
--
-- 'attributes', 'parameterObject_attributes' - The attributes of the parameter object.
newParameterObject ::
  -- | 'id'
  Prelude.Text ->
  ParameterObject
newParameterObject pId_ =
  ParameterObject'
    { id = pId_,
      attributes = Prelude.mempty
    }

-- | The ID of the parameter object.
parameterObject_id :: Lens.Lens' ParameterObject Prelude.Text
parameterObject_id = Lens.lens (\ParameterObject' {id} -> id) (\s@ParameterObject' {} a -> s {id = a} :: ParameterObject)

-- | The attributes of the parameter object.
parameterObject_attributes :: Lens.Lens' ParameterObject [ParameterAttribute]
parameterObject_attributes = Lens.lens (\ParameterObject' {attributes} -> attributes) (\s@ParameterObject' {} a -> s {attributes = a} :: ParameterObject) Prelude.. Lens.coerced

instance Data.FromJSON ParameterObject where
  parseJSON =
    Data.withObject
      "ParameterObject"
      ( \x ->
          ParameterObject'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..:? "attributes" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ParameterObject where
  hashWithSalt _salt ParameterObject' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData ParameterObject where
  rnf ParameterObject' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf attributes

instance Data.ToJSON ParameterObject where
  toJSON ParameterObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("id" Data..= id),
            Prelude.Just ("attributes" Data..= attributes)
          ]
      )
