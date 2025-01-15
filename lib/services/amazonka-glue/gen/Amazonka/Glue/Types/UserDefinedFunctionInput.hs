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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.UserDefinedFunctionInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.PrincipalType
import Amazonka.Glue.Types.ResourceUri
import qualified Amazonka.Prelude as Prelude

-- | A structure used to create or update a user-defined function.
--
-- /See:/ 'newUserDefinedFunctionInput' smart constructor.
data UserDefinedFunctionInput = UserDefinedFunctionInput'
  { -- | The Java class that contains the function code.
    className :: Prelude.Maybe Prelude.Text,
    -- | The name of the function.
    functionName :: Prelude.Maybe Prelude.Text,
    -- | The owner of the function.
    ownerName :: Prelude.Maybe Prelude.Text,
    -- | The owner type.
    ownerType :: Prelude.Maybe PrincipalType,
    -- | The resource URIs for the function.
    resourceUris :: Prelude.Maybe [ResourceUri]
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
-- 'className', 'userDefinedFunctionInput_className' - The Java class that contains the function code.
--
-- 'functionName', 'userDefinedFunctionInput_functionName' - The name of the function.
--
-- 'ownerName', 'userDefinedFunctionInput_ownerName' - The owner of the function.
--
-- 'ownerType', 'userDefinedFunctionInput_ownerType' - The owner type.
--
-- 'resourceUris', 'userDefinedFunctionInput_resourceUris' - The resource URIs for the function.
newUserDefinedFunctionInput ::
  UserDefinedFunctionInput
newUserDefinedFunctionInput =
  UserDefinedFunctionInput'
    { className =
        Prelude.Nothing,
      functionName = Prelude.Nothing,
      ownerName = Prelude.Nothing,
      ownerType = Prelude.Nothing,
      resourceUris = Prelude.Nothing
    }

-- | The Java class that contains the function code.
userDefinedFunctionInput_className :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe Prelude.Text)
userDefinedFunctionInput_className = Lens.lens (\UserDefinedFunctionInput' {className} -> className) (\s@UserDefinedFunctionInput' {} a -> s {className = a} :: UserDefinedFunctionInput)

-- | The name of the function.
userDefinedFunctionInput_functionName :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe Prelude.Text)
userDefinedFunctionInput_functionName = Lens.lens (\UserDefinedFunctionInput' {functionName} -> functionName) (\s@UserDefinedFunctionInput' {} a -> s {functionName = a} :: UserDefinedFunctionInput)

-- | The owner of the function.
userDefinedFunctionInput_ownerName :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe Prelude.Text)
userDefinedFunctionInput_ownerName = Lens.lens (\UserDefinedFunctionInput' {ownerName} -> ownerName) (\s@UserDefinedFunctionInput' {} a -> s {ownerName = a} :: UserDefinedFunctionInput)

-- | The owner type.
userDefinedFunctionInput_ownerType :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe PrincipalType)
userDefinedFunctionInput_ownerType = Lens.lens (\UserDefinedFunctionInput' {ownerType} -> ownerType) (\s@UserDefinedFunctionInput' {} a -> s {ownerType = a} :: UserDefinedFunctionInput)

-- | The resource URIs for the function.
userDefinedFunctionInput_resourceUris :: Lens.Lens' UserDefinedFunctionInput (Prelude.Maybe [ResourceUri])
userDefinedFunctionInput_resourceUris = Lens.lens (\UserDefinedFunctionInput' {resourceUris} -> resourceUris) (\s@UserDefinedFunctionInput' {} a -> s {resourceUris = a} :: UserDefinedFunctionInput) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UserDefinedFunctionInput where
  hashWithSalt _salt UserDefinedFunctionInput' {..} =
    _salt
      `Prelude.hashWithSalt` className
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` ownerName
      `Prelude.hashWithSalt` ownerType
      `Prelude.hashWithSalt` resourceUris

instance Prelude.NFData UserDefinedFunctionInput where
  rnf UserDefinedFunctionInput' {..} =
    Prelude.rnf className `Prelude.seq`
      Prelude.rnf functionName `Prelude.seq`
        Prelude.rnf ownerName `Prelude.seq`
          Prelude.rnf ownerType `Prelude.seq`
            Prelude.rnf resourceUris

instance Data.ToJSON UserDefinedFunctionInput where
  toJSON UserDefinedFunctionInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClassName" Data..=) Prelude.<$> className,
            ("FunctionName" Data..=) Prelude.<$> functionName,
            ("OwnerName" Data..=) Prelude.<$> ownerName,
            ("OwnerType" Data..=) Prelude.<$> ownerType,
            ("ResourceUris" Data..=) Prelude.<$> resourceUris
          ]
      )
