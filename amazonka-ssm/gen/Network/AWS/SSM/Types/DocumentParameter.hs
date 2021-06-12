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
-- Module      : Network.AWS.SSM.Types.DocumentParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentParameter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.DocumentParameterType

-- | Parameters specified in a System Manager document that run on the server
-- when the command is run.
--
-- /See:/ 'newDocumentParameter' smart constructor.
data DocumentParameter = DocumentParameter'
  { -- | The name of the parameter.
    name :: Core.Maybe Core.Text,
    -- | A description of what the parameter does, how to use it, the default
    -- value, and whether or not the parameter is optional.
    description :: Core.Maybe Core.Text,
    -- | The type of parameter. The type can be either String or StringList.
    type' :: Core.Maybe DocumentParameterType,
    -- | If specified, the default values for the parameters. Parameters without
    -- a default value are required. Parameters with a default value are
    -- optional.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'documentParameter_name' - The name of the parameter.
--
-- 'description', 'documentParameter_description' - A description of what the parameter does, how to use it, the default
-- value, and whether or not the parameter is optional.
--
-- 'type'', 'documentParameter_type' - The type of parameter. The type can be either String or StringList.
--
-- 'defaultValue', 'documentParameter_defaultValue' - If specified, the default values for the parameters. Parameters without
-- a default value are required. Parameters with a default value are
-- optional.
newDocumentParameter ::
  DocumentParameter
newDocumentParameter =
  DocumentParameter'
    { name = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | The name of the parameter.
documentParameter_name :: Lens.Lens' DocumentParameter (Core.Maybe Core.Text)
documentParameter_name = Lens.lens (\DocumentParameter' {name} -> name) (\s@DocumentParameter' {} a -> s {name = a} :: DocumentParameter)

-- | A description of what the parameter does, how to use it, the default
-- value, and whether or not the parameter is optional.
documentParameter_description :: Lens.Lens' DocumentParameter (Core.Maybe Core.Text)
documentParameter_description = Lens.lens (\DocumentParameter' {description} -> description) (\s@DocumentParameter' {} a -> s {description = a} :: DocumentParameter)

-- | The type of parameter. The type can be either String or StringList.
documentParameter_type :: Lens.Lens' DocumentParameter (Core.Maybe DocumentParameterType)
documentParameter_type = Lens.lens (\DocumentParameter' {type'} -> type') (\s@DocumentParameter' {} a -> s {type' = a} :: DocumentParameter)

-- | If specified, the default values for the parameters. Parameters without
-- a default value are required. Parameters with a default value are
-- optional.
documentParameter_defaultValue :: Lens.Lens' DocumentParameter (Core.Maybe Core.Text)
documentParameter_defaultValue = Lens.lens (\DocumentParameter' {defaultValue} -> defaultValue) (\s@DocumentParameter' {} a -> s {defaultValue = a} :: DocumentParameter)

instance Core.FromJSON DocumentParameter where
  parseJSON =
    Core.withObject
      "DocumentParameter"
      ( \x ->
          DocumentParameter'
            Core.<$> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "DefaultValue")
      )

instance Core.Hashable DocumentParameter

instance Core.NFData DocumentParameter
