{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.PropertyNameQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PropertyNameQuery where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Part of the @SuggestionQuery@ type. Specifies a hint for retrieving
-- property names that begin with the specified text.
--
-- /See:/ 'newPropertyNameQuery' smart constructor.
data PropertyNameQuery = PropertyNameQuery'
  { -- | Text that begins a property\'s name.
    propertyNameHint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PropertyNameQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyNameHint', 'propertyNameQuery_propertyNameHint' - Text that begins a property\'s name.
newPropertyNameQuery ::
  -- | 'propertyNameHint'
  Prelude.Text ->
  PropertyNameQuery
newPropertyNameQuery pPropertyNameHint_ =
  PropertyNameQuery'
    { propertyNameHint =
        pPropertyNameHint_
    }

-- | Text that begins a property\'s name.
propertyNameQuery_propertyNameHint :: Lens.Lens' PropertyNameQuery Prelude.Text
propertyNameQuery_propertyNameHint = Lens.lens (\PropertyNameQuery' {propertyNameHint} -> propertyNameHint) (\s@PropertyNameQuery' {} a -> s {propertyNameHint = a} :: PropertyNameQuery)

instance Prelude.Hashable PropertyNameQuery

instance Prelude.NFData PropertyNameQuery

instance Prelude.ToJSON PropertyNameQuery where
  toJSON PropertyNameQuery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PropertyNameHint" Prelude..= propertyNameHint)
          ]
      )
