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
-- Module      : Amazonka.ConnectCases.Types.ContactFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.ContactFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter for related items of type @Contact@.
--
-- /See:/ 'newContactFilter' smart constructor.
data ContactFilter = ContactFilter'
  { -- | A list of channels to filter on for related items of type @Contact@.
    channel :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier of a contact in Amazon Connect.
    contactArn :: Prelude.Maybe Prelude.Text
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
-- 'channel', 'contactFilter_channel' - A list of channels to filter on for related items of type @Contact@.
--
-- 'contactArn', 'contactFilter_contactArn' - A unique identifier of a contact in Amazon Connect.
newContactFilter ::
  ContactFilter
newContactFilter =
  ContactFilter'
    { channel = Prelude.Nothing,
      contactArn = Prelude.Nothing
    }

-- | A list of channels to filter on for related items of type @Contact@.
contactFilter_channel :: Lens.Lens' ContactFilter (Prelude.Maybe [Prelude.Text])
contactFilter_channel = Lens.lens (\ContactFilter' {channel} -> channel) (\s@ContactFilter' {} a -> s {channel = a} :: ContactFilter) Prelude.. Lens.mapping Lens.coerced

-- | A unique identifier of a contact in Amazon Connect.
contactFilter_contactArn :: Lens.Lens' ContactFilter (Prelude.Maybe Prelude.Text)
contactFilter_contactArn = Lens.lens (\ContactFilter' {contactArn} -> contactArn) (\s@ContactFilter' {} a -> s {contactArn = a} :: ContactFilter)

instance Prelude.Hashable ContactFilter where
  hashWithSalt _salt ContactFilter' {..} =
    _salt `Prelude.hashWithSalt` channel
      `Prelude.hashWithSalt` contactArn

instance Prelude.NFData ContactFilter where
  rnf ContactFilter' {..} =
    Prelude.rnf channel
      `Prelude.seq` Prelude.rnf contactArn

instance Data.ToJSON ContactFilter where
  toJSON ContactFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("channel" Data..=) Prelude.<$> channel,
            ("contactArn" Data..=) Prelude.<$> contactArn
          ]
      )
