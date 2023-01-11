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
-- Module      : Amazonka.BillingConductor.Types.AssociateResourceError
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BillingConductor.Types.AssociateResourceError where

import Amazonka.BillingConductor.Types.AssociateResourceErrorReason
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A representation of a resource association error.
--
-- /See:/ 'newAssociateResourceError' smart constructor.
data AssociateResourceError = AssociateResourceError'
  { -- | The reason why the resource association failed.
    message :: Prelude.Maybe Prelude.Text,
    -- | A static error code that\'s used to classify the type of failure.
    reason :: Prelude.Maybe AssociateResourceErrorReason
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateResourceError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'associateResourceError_message' - The reason why the resource association failed.
--
-- 'reason', 'associateResourceError_reason' - A static error code that\'s used to classify the type of failure.
newAssociateResourceError ::
  AssociateResourceError
newAssociateResourceError =
  AssociateResourceError'
    { message = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The reason why the resource association failed.
associateResourceError_message :: Lens.Lens' AssociateResourceError (Prelude.Maybe Prelude.Text)
associateResourceError_message = Lens.lens (\AssociateResourceError' {message} -> message) (\s@AssociateResourceError' {} a -> s {message = a} :: AssociateResourceError)

-- | A static error code that\'s used to classify the type of failure.
associateResourceError_reason :: Lens.Lens' AssociateResourceError (Prelude.Maybe AssociateResourceErrorReason)
associateResourceError_reason = Lens.lens (\AssociateResourceError' {reason} -> reason) (\s@AssociateResourceError' {} a -> s {reason = a} :: AssociateResourceError)

instance Data.FromJSON AssociateResourceError where
  parseJSON =
    Data.withObject
      "AssociateResourceError"
      ( \x ->
          AssociateResourceError'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Reason")
      )

instance Prelude.Hashable AssociateResourceError where
  hashWithSalt _salt AssociateResourceError' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` reason

instance Prelude.NFData AssociateResourceError where
  rnf AssociateResourceError' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf reason
