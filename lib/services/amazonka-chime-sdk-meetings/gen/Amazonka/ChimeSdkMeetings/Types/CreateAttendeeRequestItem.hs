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
-- Module      : Amazonka.ChimeSdkMeetings.Types.CreateAttendeeRequestItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMeetings.Types.CreateAttendeeRequestItem where

import Amazonka.ChimeSdkMeetings.Types.AttendeeCapabilities
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Chime SDK attendee fields to create, used with the
-- BatchCreateAttendee action.
--
-- /See:/ 'newCreateAttendeeRequestItem' smart constructor.
data CreateAttendeeRequestItem = CreateAttendeeRequestItem'
  { -- | A list of one or more capabilities.
    capabilities :: Prelude.Maybe AttendeeCapabilities,
    -- | The Amazon Chime SDK external user ID. An idempotency token. Links the
    -- attendee to an identity managed by a builder application.
    externalUserId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAttendeeRequestItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capabilities', 'createAttendeeRequestItem_capabilities' - A list of one or more capabilities.
--
-- 'externalUserId', 'createAttendeeRequestItem_externalUserId' - The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
newCreateAttendeeRequestItem ::
  -- | 'externalUserId'
  Prelude.Text ->
  CreateAttendeeRequestItem
newCreateAttendeeRequestItem pExternalUserId_ =
  CreateAttendeeRequestItem'
    { capabilities =
        Prelude.Nothing,
      externalUserId =
        Data._Sensitive Lens.# pExternalUserId_
    }

-- | A list of one or more capabilities.
createAttendeeRequestItem_capabilities :: Lens.Lens' CreateAttendeeRequestItem (Prelude.Maybe AttendeeCapabilities)
createAttendeeRequestItem_capabilities = Lens.lens (\CreateAttendeeRequestItem' {capabilities} -> capabilities) (\s@CreateAttendeeRequestItem' {} a -> s {capabilities = a} :: CreateAttendeeRequestItem)

-- | The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
createAttendeeRequestItem_externalUserId :: Lens.Lens' CreateAttendeeRequestItem Prelude.Text
createAttendeeRequestItem_externalUserId = Lens.lens (\CreateAttendeeRequestItem' {externalUserId} -> externalUserId) (\s@CreateAttendeeRequestItem' {} a -> s {externalUserId = a} :: CreateAttendeeRequestItem) Prelude.. Data._Sensitive

instance Prelude.Hashable CreateAttendeeRequestItem where
  hashWithSalt _salt CreateAttendeeRequestItem' {..} =
    _salt
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` externalUserId

instance Prelude.NFData CreateAttendeeRequestItem where
  rnf CreateAttendeeRequestItem' {..} =
    Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf externalUserId

instance Data.ToJSON CreateAttendeeRequestItem where
  toJSON CreateAttendeeRequestItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Capabilities" Data..=) Prelude.<$> capabilities,
            Prelude.Just
              ("ExternalUserId" Data..= externalUserId)
          ]
      )
