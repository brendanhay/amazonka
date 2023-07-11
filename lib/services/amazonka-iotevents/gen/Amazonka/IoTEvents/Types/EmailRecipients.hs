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
-- Module      : Amazonka.IoTEvents.Types.EmailRecipients
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.EmailRecipients where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTEvents.Types.RecipientDetail
import qualified Amazonka.Prelude as Prelude

-- | Contains the information of one or more recipients who receive the
-- emails.
--
-- You must
-- <https://docs.aws.amazon.com/singlesignon/latest/userguide/addusers.html add the users that receive emails to your AWS SSO store>.
--
-- /See:/ 'newEmailRecipients' smart constructor.
data EmailRecipients = EmailRecipients'
  { -- | Specifies one or more recipients who receive the email.
    to :: Prelude.Maybe (Prelude.NonEmpty RecipientDetail)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailRecipients' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'to', 'emailRecipients_to' - Specifies one or more recipients who receive the email.
newEmailRecipients ::
  EmailRecipients
newEmailRecipients =
  EmailRecipients' {to = Prelude.Nothing}

-- | Specifies one or more recipients who receive the email.
emailRecipients_to :: Lens.Lens' EmailRecipients (Prelude.Maybe (Prelude.NonEmpty RecipientDetail))
emailRecipients_to = Lens.lens (\EmailRecipients' {to} -> to) (\s@EmailRecipients' {} a -> s {to = a} :: EmailRecipients) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EmailRecipients where
  parseJSON =
    Data.withObject
      "EmailRecipients"
      ( \x ->
          EmailRecipients' Prelude.<$> (x Data..:? "to")
      )

instance Prelude.Hashable EmailRecipients where
  hashWithSalt _salt EmailRecipients' {..} =
    _salt `Prelude.hashWithSalt` to

instance Prelude.NFData EmailRecipients where
  rnf EmailRecipients' {..} = Prelude.rnf to

instance Data.ToJSON EmailRecipients where
  toJSON EmailRecipients' {..} =
    Data.object
      (Prelude.catMaybes [("to" Data..=) Prelude.<$> to])
