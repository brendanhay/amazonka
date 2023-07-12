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
-- Module      : Amazonka.ConnectCases.Types.Contact
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.Contact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an Amazon Connect contact object.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | A unique identifier of a contact in Amazon Connect.
    contactArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Contact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'contact_contactArn' - A unique identifier of a contact in Amazon Connect.
newContact ::
  -- | 'contactArn'
  Prelude.Text ->
  Contact
newContact pContactArn_ =
  Contact' {contactArn = pContactArn_}

-- | A unique identifier of a contact in Amazon Connect.
contact_contactArn :: Lens.Lens' Contact Prelude.Text
contact_contactArn = Lens.lens (\Contact' {contactArn} -> contactArn) (\s@Contact' {} a -> s {contactArn = a} :: Contact)

instance Prelude.Hashable Contact where
  hashWithSalt _salt Contact' {..} =
    _salt `Prelude.hashWithSalt` contactArn

instance Prelude.NFData Contact where
  rnf Contact' {..} = Prelude.rnf contactArn

instance Data.ToJSON Contact where
  toJSON Contact' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("contactArn" Data..= contactArn)]
      )
