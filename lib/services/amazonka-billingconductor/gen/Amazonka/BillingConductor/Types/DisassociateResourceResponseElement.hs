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
-- Module      : Amazonka.BillingConductor.Types.DisassociateResourceResponseElement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.DisassociateResourceResponseElement where

import Amazonka.BillingConductor.Types.AssociateResourceError
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A resource disassociation result for a percentage custom line item.
--
-- /See:/ 'newDisassociateResourceResponseElement' smart constructor.
data DisassociateResourceResponseElement = DisassociateResourceResponseElement'
  { -- | The resource ARN that was disassociated from the custom line item.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An @AssociateResourceError@ that\'s shown if the resource disassociation
    -- fails.
    error :: Prelude.Maybe AssociateResourceError
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResourceResponseElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'disassociateResourceResponseElement_arn' - The resource ARN that was disassociated from the custom line item.
--
-- 'error', 'disassociateResourceResponseElement_error' - An @AssociateResourceError@ that\'s shown if the resource disassociation
-- fails.
newDisassociateResourceResponseElement ::
  DisassociateResourceResponseElement
newDisassociateResourceResponseElement =
  DisassociateResourceResponseElement'
    { arn =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The resource ARN that was disassociated from the custom line item.
disassociateResourceResponseElement_arn :: Lens.Lens' DisassociateResourceResponseElement (Prelude.Maybe Prelude.Text)
disassociateResourceResponseElement_arn = Lens.lens (\DisassociateResourceResponseElement' {arn} -> arn) (\s@DisassociateResourceResponseElement' {} a -> s {arn = a} :: DisassociateResourceResponseElement)

-- | An @AssociateResourceError@ that\'s shown if the resource disassociation
-- fails.
disassociateResourceResponseElement_error :: Lens.Lens' DisassociateResourceResponseElement (Prelude.Maybe AssociateResourceError)
disassociateResourceResponseElement_error = Lens.lens (\DisassociateResourceResponseElement' {error} -> error) (\s@DisassociateResourceResponseElement' {} a -> s {error = a} :: DisassociateResourceResponseElement)

instance
  Core.FromJSON
    DisassociateResourceResponseElement
  where
  parseJSON =
    Core.withObject
      "DisassociateResourceResponseElement"
      ( \x ->
          DisassociateResourceResponseElement'
            Prelude.<$> (x Core..:? "Arn") Prelude.<*> (x Core..:? "Error")
      )

instance
  Prelude.Hashable
    DisassociateResourceResponseElement
  where
  hashWithSalt
    _salt
    DisassociateResourceResponseElement' {..} =
      _salt `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    DisassociateResourceResponseElement
  where
  rnf DisassociateResourceResponseElement' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf error
