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
-- Module      : Amazonka.Pinpoint.Types.ContactCenterActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.ContactCenterActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newContactCenterActivity' smart constructor.
data ContactCenterActivity = ContactCenterActivity'
  { -- | The unique identifier for the next activity to perform after the this
    -- activity.
    nextActivity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContactCenterActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextActivity', 'contactCenterActivity_nextActivity' - The unique identifier for the next activity to perform after the this
-- activity.
newContactCenterActivity ::
  ContactCenterActivity
newContactCenterActivity =
  ContactCenterActivity'
    { nextActivity =
        Prelude.Nothing
    }

-- | The unique identifier for the next activity to perform after the this
-- activity.
contactCenterActivity_nextActivity :: Lens.Lens' ContactCenterActivity (Prelude.Maybe Prelude.Text)
contactCenterActivity_nextActivity = Lens.lens (\ContactCenterActivity' {nextActivity} -> nextActivity) (\s@ContactCenterActivity' {} a -> s {nextActivity = a} :: ContactCenterActivity)

instance Data.FromJSON ContactCenterActivity where
  parseJSON =
    Data.withObject
      "ContactCenterActivity"
      ( \x ->
          ContactCenterActivity'
            Prelude.<$> (x Data..:? "NextActivity")
      )

instance Prelude.Hashable ContactCenterActivity where
  hashWithSalt _salt ContactCenterActivity' {..} =
    _salt `Prelude.hashWithSalt` nextActivity

instance Prelude.NFData ContactCenterActivity where
  rnf ContactCenterActivity' {..} =
    Prelude.rnf nextActivity

instance Data.ToJSON ContactCenterActivity where
  toJSON ContactCenterActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [("NextActivity" Data..=) Prelude.<$> nextActivity]
      )
