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
-- Module      : Amazonka.SES.Types.AddHeaderAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.AddHeaderAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When included in a receipt rule, this action adds a header to the
-- received email.
--
-- For information about adding a header using a receipt rule, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-add-header.html Amazon SES Developer Guide>.
--
-- /See:/ 'newAddHeaderAction' smart constructor.
data AddHeaderAction = AddHeaderAction'
  { -- | The name of the header to add. Must be between 1 and 50 characters,
    -- inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and
    -- dashes only.
    headerName :: Prelude.Text,
    -- | Must be less than 2048 characters, and must not contain newline
    -- characters (\"\\r\" or \"\\n\").
    headerValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddHeaderAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'headerName', 'addHeaderAction_headerName' - The name of the header to add. Must be between 1 and 50 characters,
-- inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and
-- dashes only.
--
-- 'headerValue', 'addHeaderAction_headerValue' - Must be less than 2048 characters, and must not contain newline
-- characters (\"\\r\" or \"\\n\").
newAddHeaderAction ::
  -- | 'headerName'
  Prelude.Text ->
  -- | 'headerValue'
  Prelude.Text ->
  AddHeaderAction
newAddHeaderAction pHeaderName_ pHeaderValue_ =
  AddHeaderAction'
    { headerName = pHeaderName_,
      headerValue = pHeaderValue_
    }

-- | The name of the header to add. Must be between 1 and 50 characters,
-- inclusive, and consist of alphanumeric (a-z, A-Z, 0-9) characters and
-- dashes only.
addHeaderAction_headerName :: Lens.Lens' AddHeaderAction Prelude.Text
addHeaderAction_headerName = Lens.lens (\AddHeaderAction' {headerName} -> headerName) (\s@AddHeaderAction' {} a -> s {headerName = a} :: AddHeaderAction)

-- | Must be less than 2048 characters, and must not contain newline
-- characters (\"\\r\" or \"\\n\").
addHeaderAction_headerValue :: Lens.Lens' AddHeaderAction Prelude.Text
addHeaderAction_headerValue = Lens.lens (\AddHeaderAction' {headerValue} -> headerValue) (\s@AddHeaderAction' {} a -> s {headerValue = a} :: AddHeaderAction)

instance Data.FromXML AddHeaderAction where
  parseXML x =
    AddHeaderAction'
      Prelude.<$> (x Data..@ "HeaderName")
      Prelude.<*> (x Data..@ "HeaderValue")

instance Prelude.Hashable AddHeaderAction where
  hashWithSalt _salt AddHeaderAction' {..} =
    _salt
      `Prelude.hashWithSalt` headerName
      `Prelude.hashWithSalt` headerValue

instance Prelude.NFData AddHeaderAction where
  rnf AddHeaderAction' {..} =
    Prelude.rnf headerName
      `Prelude.seq` Prelude.rnf headerValue

instance Data.ToQuery AddHeaderAction where
  toQuery AddHeaderAction' {..} =
    Prelude.mconcat
      [ "HeaderName" Data.=: headerName,
        "HeaderValue" Data.=: headerValue
      ]
