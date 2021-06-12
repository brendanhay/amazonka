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
-- Module      : Network.AWS.Glue.Types.UserDefinedFunctionInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UserDefinedFunctionInput where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceUri
import qualified Network.AWS.Lens as Lens

-- | A structure used to create or update a user-defined function.
--
-- /See:/ 'newUserDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { -- | The owner type.
    ownerType :: Core.Maybe PrincipalType,
    -- | The Java class that contains the function code.
    className :: Core.Maybe Core.Text,
    -- | The owner of the function.
    ownerName :: Core.Maybe Core.Text,
    -- | The name of the function.
    functionName :: Core.Maybe Core.Text,
    -- | The resource URIs for the function.
    resourceUris :: Core.Maybe [ResourceUri]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserDefinedFunctionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerType', 'userDefinedFunctionInput_ownerType' - The owner type.
--
-- 'className', 'userDefinedFunctionInput_className' - The Java class that contains the function code.
--
-- 'ownerName', 'userDefinedFunctionInput_ownerName' - The owner of the function.
--
-- 'functionName', 'userDefinedFunctionInput_functionName' - The name of the function.
--
-- 'resourceUris', 'userDefinedFunctionInput_resourceUris' - The resource URIs for the function.
newUserDefinedFunctionInput ::
  UserDefinedFunctionInput
newUserDefinedFunctionInput =
  UserDefinedFunctionInput'
    { ownerType = Core.Nothing,
      className = Core.Nothing,
      ownerName = Core.Nothing,
      functionName = Core.Nothing,
      resourceUris = Core.Nothing
    }

-- | The owner type.
userDefinedFunctionInput_ownerType :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe PrincipalType)
userDefinedFunctionInput_ownerType = Lens.lens (\UserDefinedFunctionInput' {ownerType} -> ownerType) (\s@UserDefinedFunctionInput' {} a -> s {ownerType = a} :: UserDefinedFunctionInput)

-- | The Java class that contains the function code.
userDefinedFunctionInput_className :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Core.Text)
userDefinedFunctionInput_className = Lens.lens (\UserDefinedFunctionInput' {className} -> className) (\s@UserDefinedFunctionInput' {} a -> s {className = a} :: UserDefinedFunctionInput)

-- | The owner of the function.
userDefinedFunctionInput_ownerName :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Core.Text)
userDefinedFunctionInput_ownerName = Lens.lens (\UserDefinedFunctionInput' {ownerName} -> ownerName) (\s@UserDefinedFunctionInput' {} a -> s {ownerName = a} :: UserDefinedFunctionInput)

-- | The name of the function.
userDefinedFunctionInput_functionName :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe Core.Text)
userDefinedFunctionInput_functionName = Lens.lens (\UserDefinedFunctionInput' {functionName} -> functionName) (\s@UserDefinedFunctionInput' {} a -> s {functionName = a} :: UserDefinedFunctionInput)

-- | The resource URIs for the function.
userDefinedFunctionInput_resourceUris :: Lens.Lens' UserDefinedFunctionInput (Core.Maybe [ResourceUri])
userDefinedFunctionInput_resourceUris = Lens.lens (\UserDefinedFunctionInput' {resourceUris} -> resourceUris) (\s@UserDefinedFunctionInput' {} a -> s {resourceUris = a} :: UserDefinedFunctionInput) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable UserDefinedFunctionInput

instance Core.NFData UserDefinedFunctionInput

instance Core.ToJSON UserDefinedFunctionInput where
  toJSON UserDefinedFunctionInput' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OwnerType" Core..=) Core.<$> ownerType,
            ("ClassName" Core..=) Core.<$> className,
            ("OwnerName" Core..=) Core.<$> ownerName,
            ("FunctionName" Core..=) Core.<$> functionName,
            ("ResourceUris" Core..=) Core.<$> resourceUris
          ]
      )
