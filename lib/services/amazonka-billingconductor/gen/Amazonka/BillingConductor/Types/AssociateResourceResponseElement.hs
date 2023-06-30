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
-- Module      : Amazonka.BillingConductor.Types.AssociateResourceResponseElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.AssociateResourceResponseElement where

import Amazonka.BillingConductor.Types.AssociateResourceError
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A resource association result for a percentage custom line item.
--
-- /See:/ 'newAssociateResourceResponseElement' smart constructor.
data AssociateResourceResponseElement = AssociateResourceResponseElement'
  { -- | The resource ARN that was associated to the custom line item.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An @AssociateResourceError@ that will populate if the resource
    -- association fails.
    error :: Prelude.Maybe AssociateResourceError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceResponseElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'associateResourceResponseElement_arn' - The resource ARN that was associated to the custom line item.
--
-- 'error', 'associateResourceResponseElement_error' - An @AssociateResourceError@ that will populate if the resource
-- association fails.
newAssociateResourceResponseElement ::
  AssociateResourceResponseElement
newAssociateResourceResponseElement =
  AssociateResourceResponseElement'
    { arn =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The resource ARN that was associated to the custom line item.
associateResourceResponseElement_arn :: Lens.Lens' AssociateResourceResponseElement (Prelude.Maybe Prelude.Text)
associateResourceResponseElement_arn = Lens.lens (\AssociateResourceResponseElement' {arn} -> arn) (\s@AssociateResourceResponseElement' {} a -> s {arn = a} :: AssociateResourceResponseElement)

-- | An @AssociateResourceError@ that will populate if the resource
-- association fails.
associateResourceResponseElement_error :: Lens.Lens' AssociateResourceResponseElement (Prelude.Maybe AssociateResourceError)
associateResourceResponseElement_error = Lens.lens (\AssociateResourceResponseElement' {error} -> error) (\s@AssociateResourceResponseElement' {} a -> s {error = a} :: AssociateResourceResponseElement)

instance
  Data.FromJSON
    AssociateResourceResponseElement
  where
  parseJSON =
    Data.withObject
      "AssociateResourceResponseElement"
      ( \x ->
          AssociateResourceResponseElement'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Error")
      )

instance
  Prelude.Hashable
    AssociateResourceResponseElement
  where
  hashWithSalt
    _salt
    AssociateResourceResponseElement' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    AssociateResourceResponseElement
  where
  rnf AssociateResourceResponseElement' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf error
