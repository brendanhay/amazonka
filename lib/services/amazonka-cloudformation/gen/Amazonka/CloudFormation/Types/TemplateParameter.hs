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
-- Module      : Amazonka.CloudFormation.Types.TemplateParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.TemplateParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The TemplateParameter data type.
--
-- /See:/ 'newTemplateParameter' smart constructor.
data TemplateParameter = TemplateParameter'
  { -- | The default value associated with the parameter.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | User defined description associated with the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | Flag indicating whether the parameter should be displayed as plain text
    -- in logs and UIs.
    noEcho :: Prelude.Maybe Prelude.Bool,
    -- | The name associated with the parameter.
    parameterKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'templateParameter_defaultValue' - The default value associated with the parameter.
--
-- 'description', 'templateParameter_description' - User defined description associated with the parameter.
--
-- 'noEcho', 'templateParameter_noEcho' - Flag indicating whether the parameter should be displayed as plain text
-- in logs and UIs.
--
-- 'parameterKey', 'templateParameter_parameterKey' - The name associated with the parameter.
newTemplateParameter ::
  TemplateParameter
newTemplateParameter =
  TemplateParameter'
    { defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      noEcho = Prelude.Nothing,
      parameterKey = Prelude.Nothing
    }

-- | The default value associated with the parameter.
templateParameter_defaultValue :: Lens.Lens' TemplateParameter (Prelude.Maybe Prelude.Text)
templateParameter_defaultValue = Lens.lens (\TemplateParameter' {defaultValue} -> defaultValue) (\s@TemplateParameter' {} a -> s {defaultValue = a} :: TemplateParameter)

-- | User defined description associated with the parameter.
templateParameter_description :: Lens.Lens' TemplateParameter (Prelude.Maybe Prelude.Text)
templateParameter_description = Lens.lens (\TemplateParameter' {description} -> description) (\s@TemplateParameter' {} a -> s {description = a} :: TemplateParameter)

-- | Flag indicating whether the parameter should be displayed as plain text
-- in logs and UIs.
templateParameter_noEcho :: Lens.Lens' TemplateParameter (Prelude.Maybe Prelude.Bool)
templateParameter_noEcho = Lens.lens (\TemplateParameter' {noEcho} -> noEcho) (\s@TemplateParameter' {} a -> s {noEcho = a} :: TemplateParameter)

-- | The name associated with the parameter.
templateParameter_parameterKey :: Lens.Lens' TemplateParameter (Prelude.Maybe Prelude.Text)
templateParameter_parameterKey = Lens.lens (\TemplateParameter' {parameterKey} -> parameterKey) (\s@TemplateParameter' {} a -> s {parameterKey = a} :: TemplateParameter)

instance Data.FromXML TemplateParameter where
  parseXML x =
    TemplateParameter'
      Prelude.<$> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "Description")
      Prelude.<*> (x Data..@? "NoEcho")
      Prelude.<*> (x Data..@? "ParameterKey")

instance Prelude.Hashable TemplateParameter where
  hashWithSalt _salt TemplateParameter' {..} =
    _salt
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` noEcho
      `Prelude.hashWithSalt` parameterKey

instance Prelude.NFData TemplateParameter where
  rnf TemplateParameter' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf noEcho
      `Prelude.seq` Prelude.rnf parameterKey
