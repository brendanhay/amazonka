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
-- Module      : Network.AWS.Pinpoint.Types.ContactCenterActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ContactCenterActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON ContactCenterActivity where
  parseJSON =
    Core.withObject
      "ContactCenterActivity"
      ( \x ->
          ContactCenterActivity'
            Prelude.<$> (x Core..:? "NextActivity")
      )

instance Prelude.Hashable ContactCenterActivity

instance Prelude.NFData ContactCenterActivity

instance Core.ToJSON ContactCenterActivity where
  toJSON ContactCenterActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [("NextActivity" Core..=) Prelude.<$> nextActivity]
      )
