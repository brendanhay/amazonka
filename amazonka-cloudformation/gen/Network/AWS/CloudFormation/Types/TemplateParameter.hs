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
-- Module      : Network.AWS.CloudFormation.Types.TemplateParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TemplateParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The TemplateParameter data type.
--
-- /See:/ 'newTemplateParameter' smart constructor.
data TemplateParameter = TemplateParameter'
  { -- | The name associated with the parameter.
    parameterKey :: Core.Maybe Core.Text,
    -- | User defined description associated with the parameter.
    description :: Core.Maybe Core.Text,
    -- | Flag indicating whether the parameter should be displayed as plain text
    -- in logs and UIs.
    noEcho :: Core.Maybe Core.Bool,
    -- | The default value associated with the parameter.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TemplateParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterKey', 'templateParameter_parameterKey' - The name associated with the parameter.
--
-- 'description', 'templateParameter_description' - User defined description associated with the parameter.
--
-- 'noEcho', 'templateParameter_noEcho' - Flag indicating whether the parameter should be displayed as plain text
-- in logs and UIs.
--
-- 'defaultValue', 'templateParameter_defaultValue' - The default value associated with the parameter.
newTemplateParameter ::
  TemplateParameter
newTemplateParameter =
  TemplateParameter'
    { parameterKey = Core.Nothing,
      description = Core.Nothing,
      noEcho = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | The name associated with the parameter.
templateParameter_parameterKey :: Lens.Lens' TemplateParameter (Core.Maybe Core.Text)
templateParameter_parameterKey = Lens.lens (\TemplateParameter' {parameterKey} -> parameterKey) (\s@TemplateParameter' {} a -> s {parameterKey = a} :: TemplateParameter)

-- | User defined description associated with the parameter.
templateParameter_description :: Lens.Lens' TemplateParameter (Core.Maybe Core.Text)
templateParameter_description = Lens.lens (\TemplateParameter' {description} -> description) (\s@TemplateParameter' {} a -> s {description = a} :: TemplateParameter)

-- | Flag indicating whether the parameter should be displayed as plain text
-- in logs and UIs.
templateParameter_noEcho :: Lens.Lens' TemplateParameter (Core.Maybe Core.Bool)
templateParameter_noEcho = Lens.lens (\TemplateParameter' {noEcho} -> noEcho) (\s@TemplateParameter' {} a -> s {noEcho = a} :: TemplateParameter)

-- | The default value associated with the parameter.
templateParameter_defaultValue :: Lens.Lens' TemplateParameter (Core.Maybe Core.Text)
templateParameter_defaultValue = Lens.lens (\TemplateParameter' {defaultValue} -> defaultValue) (\s@TemplateParameter' {} a -> s {defaultValue = a} :: TemplateParameter)

instance Core.FromXML TemplateParameter where
  parseXML x =
    TemplateParameter'
      Core.<$> (x Core..@? "ParameterKey")
      Core.<*> (x Core..@? "Description")
      Core.<*> (x Core..@? "NoEcho")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable TemplateParameter

instance Core.NFData TemplateParameter
