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
-- Module      : Amazonka.Kendra.Types.TemplateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.TemplateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.Template
import qualified Amazonka.Prelude as Prelude

-- | Provides a template for the configuration information to connect to your
-- data source.
--
-- /See:/ 'newTemplateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { -- | The template schema used for the data source, where templates schemas
    -- are supported.
    --
    -- See
    -- <https://docs.aws.amazon.com/kendra/latest/dg/ds-schemas.html Data source template schemas>.
    template :: Prelude.Maybe Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'template', 'templateConfiguration_template' - The template schema used for the data source, where templates schemas
-- are supported.
--
-- See
-- <https://docs.aws.amazon.com/kendra/latest/dg/ds-schemas.html Data source template schemas>.
newTemplateConfiguration ::
  TemplateConfiguration
newTemplateConfiguration =
  TemplateConfiguration' {template = Prelude.Nothing}

-- | The template schema used for the data source, where templates schemas
-- are supported.
--
-- See
-- <https://docs.aws.amazon.com/kendra/latest/dg/ds-schemas.html Data source template schemas>.
templateConfiguration_template :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_template = Lens.lens (\TemplateConfiguration' {template} -> template) (\s@TemplateConfiguration' {} a -> s {template = a} :: TemplateConfiguration)

instance Data.FromJSON TemplateConfiguration where
  parseJSON =
    Data.withObject
      "TemplateConfiguration"
      ( \x ->
          TemplateConfiguration'
            Prelude.<$> (x Data..:? "Template")
      )

instance Prelude.Hashable TemplateConfiguration where
  hashWithSalt _salt TemplateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` template

instance Prelude.NFData TemplateConfiguration where
  rnf TemplateConfiguration' {..} = Prelude.rnf template

instance Data.ToJSON TemplateConfiguration where
  toJSON TemplateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Template" Data..=) Prelude.<$> template]
      )
