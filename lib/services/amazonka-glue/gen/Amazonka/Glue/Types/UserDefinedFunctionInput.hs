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
-- Module      : Amazonka.Glue.Types.UserDefinedFunctionInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UserDefinedFunctionInput where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.PrincipalType
import Amazonka.Glue.Types.ResourceUri
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure used to create or update a user-defined function.
--
-- /See:/ 'newUserDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { -- | The owner of the function.
    ownerName :: Prelude.Maybe Prelude.Text,
    -- | The resource URIs for the function.
    resourceUris :: Prelude.Maybe [ResourceUri],
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The owner type.
    ownerType :: Prelude.Maybe PrincipalType,
    -- | The Java class that contains the function code.
    className :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDefinedFunctionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerName', 'userDefinedFunctionInput_ownerName' - The owner of the function.
--
-- 'resourceUris', 'userDefinedFunctionInput_resourceUris' - The resource URIs for the function.
--
-- 'functionName', 'userDefinedFunctionInput_functionName' - The name of the function.
--
-- 'ownerType', 'userDefinedFunctionInput_ownerType' - The owner type.
--
-- 'className', 'userDefinedFunctionInput_className' - The Java class that contains the function code.
newUserDefinedFunctionInput ::
  UserDefinedFunctionInput
newUserDefinedFunctionInput =
  UserDefinedFunctionInput'
    { ownerName =
        Prelude.Nothing,
      resourceUris = Prelude.Nothing,
      functionName = Prelude.Nothing,
      ownerType = Prelude.Nothing,
      className = Prelude.Nothing
    }

-- | The owner of the function.
userDefinedFunctionInput_ownerName :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe Prelude.Text)
userDefinedFunctionInput_ownerName = Lens.lens (\UserDefinedFunctionInput' {ownerName} -> ownerName) (\s@UserDefinedFunctionInput' {} a -> s {ownerName = a} :: UserDefinedFunctionInput)

-- | The resource URIs for the function.
userDefinedFunctionInput_resourceUris :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe [ResourceUri])
userDefinedFunctionInput_resourceUris = Lens.lens (\UserDefinedFunctionInput' {resourceUris} -> resourceUris) (\s@UserDefinedFunctionInput' {} a -> s {resourceUris = a} :: UserDefinedFunctionInput) Prelude.. Lens.mapping Lens.coerced

-- | The name of the function.
userDefinedFunctionInput_functionName :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe Prelude.Text)
userDefinedFunctionInput_functionName = Lens.lens (\UserDefinedFunctionInput' {functionName} -> functionName) (\s@UserDefinedFunctionInput' {} a -> s {functionName = a} :: UserDefinedFunctionInput)

-- | The owner type.
userDefinedFunctionInput_ownerType :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe PrincipalType)
userDefinedFunctionInput_ownerType = Lens.lens (\UserDefinedFunctionInput' {ownerType} -> ownerType) (\s@UserDefinedFunctionInput' {} a -> s {ownerType = a} :: UserDefinedFunctionInput)

-- | The Java class that contains the function code.
userDefinedFunctionInput_className :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe Prelude.Text)
userDefinedFunctionInput_className = Lens.lens (\UserDefinedFunctionInput' {className} -> className) (\s@UserDefinedFunctionInput' {} a -> s {className = a} :: UserDefinedFunctionInput)

instance Prelude.Hashable UserDefinedFunctionInput

instance Prelude.NFData UserDefinedFunctionInput

instance Core.ToJSON UserDefinedFunctionInput where
  toJSON UserDefinedFunctionInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OwnerName" Core..=) Prelude.<$> ownerName,
            ("ResourceUris" Core..=) Prelude.<$> resourceUris,
            ("FunctionName" Core..=) Prelude.<$> functionName,
            ("OwnerType" Core..=) Prelude.<$> ownerType,
            ("ClassName" Core..=) Prelude.<$> className
          ]
      )
