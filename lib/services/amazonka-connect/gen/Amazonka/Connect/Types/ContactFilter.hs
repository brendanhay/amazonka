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
-- Module      : Amazonka.Connect.Types.ContactFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ContactFilter where

import Amazonka.Connect.Types.ContactState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Filters user data based on the contact information that is associated to
-- the users. It contains a list of
-- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html contact states>.
--
-- /See:/ 'newContactFilter' smart constructor.
data ContactFilter = ContactFilter'
  { -- | A list of up to 9
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html contact states>.
    contactStates :: Prelude.Maybe [ContactState]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactStates', 'contactFilter_contactStates' - A list of up to 9
-- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html contact states>.
newContactFilter ::
  ContactFilter
newContactFilter =
  ContactFilter' {contactStates = Prelude.Nothing}

-- | A list of up to 9
-- <https://docs.aws.amazon.com/connect/latest/adminguide/about-contact-states.html contact states>.
contactFilter_contactStates :: Lens.Lens' ContactFilter (Prelude.Maybe [ContactState])
contactFilter_contactStates = Lens.lens (\ContactFilter' {contactStates} -> contactStates) (\s@ContactFilter' {} a -> s {contactStates = a} :: ContactFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ContactFilter where
  hashWithSalt _salt ContactFilter' {..} =
    _salt `Prelude.hashWithSalt` contactStates

instance Prelude.NFData ContactFilter where
  rnf ContactFilter' {..} = Prelude.rnf contactStates

instance Data.ToJSON ContactFilter where
  toJSON ContactFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContactStates" Data..=)
              Prelude.<$> contactStates
          ]
      )
