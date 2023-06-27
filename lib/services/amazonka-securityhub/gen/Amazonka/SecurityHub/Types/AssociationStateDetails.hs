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
-- Module      : Amazonka.SecurityHub.Types.AssociationStateDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AssociationStateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of an association between a route table and a subnet
-- or gateway.
--
-- /See:/ 'newAssociationStateDetails' smart constructor.
data AssociationStateDetails = AssociationStateDetails'
  { -- | The state of the association.
    state :: Prelude.Maybe Prelude.Text,
    -- | The status message, if applicable.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociationStateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'associationStateDetails_state' - The state of the association.
--
-- 'statusMessage', 'associationStateDetails_statusMessage' - The status message, if applicable.
newAssociationStateDetails ::
  AssociationStateDetails
newAssociationStateDetails =
  AssociationStateDetails'
    { state = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The state of the association.
associationStateDetails_state :: Lens.Lens' AssociationStateDetails (Prelude.Maybe Prelude.Text)
associationStateDetails_state = Lens.lens (\AssociationStateDetails' {state} -> state) (\s@AssociationStateDetails' {} a -> s {state = a} :: AssociationStateDetails)

-- | The status message, if applicable.
associationStateDetails_statusMessage :: Lens.Lens' AssociationStateDetails (Prelude.Maybe Prelude.Text)
associationStateDetails_statusMessage = Lens.lens (\AssociationStateDetails' {statusMessage} -> statusMessage) (\s@AssociationStateDetails' {} a -> s {statusMessage = a} :: AssociationStateDetails)

instance Data.FromJSON AssociationStateDetails where
  parseJSON =
    Data.withObject
      "AssociationStateDetails"
      ( \x ->
          AssociationStateDetails'
            Prelude.<$> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable AssociationStateDetails where
  hashWithSalt _salt AssociationStateDetails' {..} =
    _salt
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData AssociationStateDetails where
  rnf AssociationStateDetails' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf statusMessage

instance Data.ToJSON AssociationStateDetails where
  toJSON AssociationStateDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("State" Data..=) Prelude.<$> state,
            ("StatusMessage" Data..=) Prelude.<$> statusMessage
          ]
      )
