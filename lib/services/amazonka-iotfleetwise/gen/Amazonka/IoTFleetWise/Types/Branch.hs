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
-- Module      : Amazonka.IoTFleetWise.Types.Branch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.Branch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A group of signals that are defined in a hierarchical structure.
--
-- /See:/ 'newBranch' smart constructor.
data Branch = Branch'
  { -- | A comment in addition to the description.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The deprecation message for the node or the branch that was moved or
    -- deleted.
    deprecationMessage :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the branch.
    description :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified name of the branch. For example, the fully qualified
    -- name of a branch might be @Vehicle.Body.Engine@.
    fullyQualifiedName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Branch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'branch_comment' - A comment in addition to the description.
--
-- 'deprecationMessage', 'branch_deprecationMessage' - The deprecation message for the node or the branch that was moved or
-- deleted.
--
-- 'description', 'branch_description' - A brief description of the branch.
--
-- 'fullyQualifiedName', 'branch_fullyQualifiedName' - The fully qualified name of the branch. For example, the fully qualified
-- name of a branch might be @Vehicle.Body.Engine@.
newBranch ::
  -- | 'fullyQualifiedName'
  Prelude.Text ->
  Branch
newBranch pFullyQualifiedName_ =
  Branch'
    { comment = Prelude.Nothing,
      deprecationMessage = Prelude.Nothing,
      description = Prelude.Nothing,
      fullyQualifiedName = pFullyQualifiedName_
    }

-- | A comment in addition to the description.
branch_comment :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_comment = Lens.lens (\Branch' {comment} -> comment) (\s@Branch' {} a -> s {comment = a} :: Branch)

-- | The deprecation message for the node or the branch that was moved or
-- deleted.
branch_deprecationMessage :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_deprecationMessage = Lens.lens (\Branch' {deprecationMessage} -> deprecationMessage) (\s@Branch' {} a -> s {deprecationMessage = a} :: Branch)

-- | A brief description of the branch.
branch_description :: Lens.Lens' Branch (Prelude.Maybe Prelude.Text)
branch_description = Lens.lens (\Branch' {description} -> description) (\s@Branch' {} a -> s {description = a} :: Branch)

-- | The fully qualified name of the branch. For example, the fully qualified
-- name of a branch might be @Vehicle.Body.Engine@.
branch_fullyQualifiedName :: Lens.Lens' Branch Prelude.Text
branch_fullyQualifiedName = Lens.lens (\Branch' {fullyQualifiedName} -> fullyQualifiedName) (\s@Branch' {} a -> s {fullyQualifiedName = a} :: Branch)

instance Data.FromJSON Branch where
  parseJSON =
    Data.withObject
      "Branch"
      ( \x ->
          Branch'
            Prelude.<$> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "deprecationMessage")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..: "fullyQualifiedName")
      )

instance Prelude.Hashable Branch where
  hashWithSalt _salt Branch' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` deprecationMessage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` fullyQualifiedName

instance Prelude.NFData Branch where
  rnf Branch' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf deprecationMessage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf fullyQualifiedName

instance Data.ToJSON Branch where
  toJSON Branch' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            ("deprecationMessage" Data..=)
              Prelude.<$> deprecationMessage,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just
              ("fullyQualifiedName" Data..= fullyQualifiedName)
          ]
      )
