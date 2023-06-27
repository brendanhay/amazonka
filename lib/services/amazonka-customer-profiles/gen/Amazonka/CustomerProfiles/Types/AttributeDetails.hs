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
-- Module      : Amazonka.CustomerProfiles.Types.AttributeDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AttributeDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.AttributeItem
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Mathematical expression and a list of attribute items specified in that
-- expression.
--
-- /See:/ 'newAttributeDetails' smart constructor.
data AttributeDetails = AttributeDetails'
  { -- | A list of attribute items specified in the mathematical expression.
    attributes :: Prelude.NonEmpty AttributeItem,
    -- | Mathematical expression that is performed on attribute items provided in
    -- the attribute list. Each element in the expression should follow the
    -- structure of \\\"{ObjectTypeName.AttributeName}\\\".
    expression :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'attributeDetails_attributes' - A list of attribute items specified in the mathematical expression.
--
-- 'expression', 'attributeDetails_expression' - Mathematical expression that is performed on attribute items provided in
-- the attribute list. Each element in the expression should follow the
-- structure of \\\"{ObjectTypeName.AttributeName}\\\".
newAttributeDetails ::
  -- | 'attributes'
  Prelude.NonEmpty AttributeItem ->
  -- | 'expression'
  Prelude.Text ->
  AttributeDetails
newAttributeDetails pAttributes_ pExpression_ =
  AttributeDetails'
    { attributes =
        Lens.coerced Lens.# pAttributes_,
      expression = pExpression_
    }

-- | A list of attribute items specified in the mathematical expression.
attributeDetails_attributes :: Lens.Lens' AttributeDetails (Prelude.NonEmpty AttributeItem)
attributeDetails_attributes = Lens.lens (\AttributeDetails' {attributes} -> attributes) (\s@AttributeDetails' {} a -> s {attributes = a} :: AttributeDetails) Prelude.. Lens.coerced

-- | Mathematical expression that is performed on attribute items provided in
-- the attribute list. Each element in the expression should follow the
-- structure of \\\"{ObjectTypeName.AttributeName}\\\".
attributeDetails_expression :: Lens.Lens' AttributeDetails Prelude.Text
attributeDetails_expression = Lens.lens (\AttributeDetails' {expression} -> expression) (\s@AttributeDetails' {} a -> s {expression = a} :: AttributeDetails)

instance Data.FromJSON AttributeDetails where
  parseJSON =
    Data.withObject
      "AttributeDetails"
      ( \x ->
          AttributeDetails'
            Prelude.<$> (x Data..: "Attributes")
            Prelude.<*> (x Data..: "Expression")
      )

instance Prelude.Hashable AttributeDetails where
  hashWithSalt _salt AttributeDetails' {..} =
    _salt
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` expression

instance Prelude.NFData AttributeDetails where
  rnf AttributeDetails' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf expression

instance Data.ToJSON AttributeDetails where
  toJSON AttributeDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Attributes" Data..= attributes),
            Prelude.Just ("Expression" Data..= expression)
          ]
      )
