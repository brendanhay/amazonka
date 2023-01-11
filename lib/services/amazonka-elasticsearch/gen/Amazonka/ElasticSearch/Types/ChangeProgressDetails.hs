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
-- Module      : Amazonka.ElasticSearch.Types.ChangeProgressDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ChangeProgressDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies change details of the domain configuration change.
--
-- /See:/ 'newChangeProgressDetails' smart constructor.
data ChangeProgressDetails = ChangeProgressDetails'
  { -- | The unique change identifier associated with a specific domain
    -- configuration change.
    changeId :: Prelude.Maybe Prelude.Text,
    -- | Contains an optional message associated with the domain configuration
    -- change.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeProgressDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeId', 'changeProgressDetails_changeId' - The unique change identifier associated with a specific domain
-- configuration change.
--
-- 'message', 'changeProgressDetails_message' - Contains an optional message associated with the domain configuration
-- change.
newChangeProgressDetails ::
  ChangeProgressDetails
newChangeProgressDetails =
  ChangeProgressDetails'
    { changeId = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The unique change identifier associated with a specific domain
-- configuration change.
changeProgressDetails_changeId :: Lens.Lens' ChangeProgressDetails (Prelude.Maybe Prelude.Text)
changeProgressDetails_changeId = Lens.lens (\ChangeProgressDetails' {changeId} -> changeId) (\s@ChangeProgressDetails' {} a -> s {changeId = a} :: ChangeProgressDetails)

-- | Contains an optional message associated with the domain configuration
-- change.
changeProgressDetails_message :: Lens.Lens' ChangeProgressDetails (Prelude.Maybe Prelude.Text)
changeProgressDetails_message = Lens.lens (\ChangeProgressDetails' {message} -> message) (\s@ChangeProgressDetails' {} a -> s {message = a} :: ChangeProgressDetails)

instance Data.FromJSON ChangeProgressDetails where
  parseJSON =
    Data.withObject
      "ChangeProgressDetails"
      ( \x ->
          ChangeProgressDetails'
            Prelude.<$> (x Data..:? "ChangeId")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable ChangeProgressDetails where
  hashWithSalt _salt ChangeProgressDetails' {..} =
    _salt `Prelude.hashWithSalt` changeId
      `Prelude.hashWithSalt` message

instance Prelude.NFData ChangeProgressDetails where
  rnf ChangeProgressDetails' {..} =
    Prelude.rnf changeId
      `Prelude.seq` Prelude.rnf message
