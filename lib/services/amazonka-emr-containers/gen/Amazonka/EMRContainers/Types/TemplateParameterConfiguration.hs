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
-- Module      : Amazonka.EMRContainers.Types.TemplateParameterConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.TemplateParameterConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types.TemplateParameterDataType
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a job template parameter.
--
-- /See:/ 'newTemplateParameterConfiguration' smart constructor.
data TemplateParameterConfiguration = TemplateParameterConfiguration'
  { -- | The default value for the job template parameter.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | The type of the job template parameter. Allowed values are: ‘String’,
    -- ‘Number’.
    type' :: Prelude.Maybe TemplateParameterDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateParameterConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'templateParameterConfiguration_defaultValue' - The default value for the job template parameter.
--
-- 'type'', 'templateParameterConfiguration_type' - The type of the job template parameter. Allowed values are: ‘String’,
-- ‘Number’.
newTemplateParameterConfiguration ::
  TemplateParameterConfiguration
newTemplateParameterConfiguration =
  TemplateParameterConfiguration'
    { defaultValue =
        Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The default value for the job template parameter.
templateParameterConfiguration_defaultValue :: Lens.Lens' TemplateParameterConfiguration (Prelude.Maybe Prelude.Text)
templateParameterConfiguration_defaultValue = Lens.lens (\TemplateParameterConfiguration' {defaultValue} -> defaultValue) (\s@TemplateParameterConfiguration' {} a -> s {defaultValue = a} :: TemplateParameterConfiguration)

-- | The type of the job template parameter. Allowed values are: ‘String’,
-- ‘Number’.
templateParameterConfiguration_type :: Lens.Lens' TemplateParameterConfiguration (Prelude.Maybe TemplateParameterDataType)
templateParameterConfiguration_type = Lens.lens (\TemplateParameterConfiguration' {type'} -> type') (\s@TemplateParameterConfiguration' {} a -> s {type' = a} :: TemplateParameterConfiguration)

instance Data.FromJSON TemplateParameterConfiguration where
  parseJSON =
    Data.withObject
      "TemplateParameterConfiguration"
      ( \x ->
          TemplateParameterConfiguration'
            Prelude.<$> (x Data..:? "defaultValue")
            Prelude.<*> (x Data..:? "type")
      )

instance
  Prelude.Hashable
    TemplateParameterConfiguration
  where
  hashWithSalt
    _salt
    TemplateParameterConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` defaultValue
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    TemplateParameterConfiguration
  where
  rnf TemplateParameterConfiguration' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON TemplateParameterConfiguration where
  toJSON TemplateParameterConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("defaultValue" Data..=) Prelude.<$> defaultValue,
            ("type" Data..=) Prelude.<$> type'
          ]
      )
