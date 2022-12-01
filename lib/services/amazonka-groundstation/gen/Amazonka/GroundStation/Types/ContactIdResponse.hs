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
-- Module      : Amazonka.GroundStation.Types.ContactIdResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ContactIdResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newContactIdResponse' smart constructor.
data ContactIdResponse = ContactIdResponse'
  { -- | UUID of a contact.
    contactId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactId', 'contactIdResponse_contactId' - UUID of a contact.
newContactIdResponse ::
  ContactIdResponse
newContactIdResponse =
  ContactIdResponse' {contactId = Prelude.Nothing}

-- | UUID of a contact.
contactIdResponse_contactId :: Lens.Lens' ContactIdResponse (Prelude.Maybe Prelude.Text)
contactIdResponse_contactId = Lens.lens (\ContactIdResponse' {contactId} -> contactId) (\s@ContactIdResponse' {} a -> s {contactId = a} :: ContactIdResponse)

instance Core.FromJSON ContactIdResponse where
  parseJSON =
    Core.withObject
      "ContactIdResponse"
      ( \x ->
          ContactIdResponse'
            Prelude.<$> (x Core..:? "contactId")
      )

instance Prelude.Hashable ContactIdResponse where
  hashWithSalt _salt ContactIdResponse' {..} =
    _salt `Prelude.hashWithSalt` contactId

instance Prelude.NFData ContactIdResponse where
  rnf ContactIdResponse' {..} = Prelude.rnf contactId
