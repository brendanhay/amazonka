{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSdkVoice.UpdatePhoneNumberSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.UpdatePhoneNumberSettings
  ( -- * Creating a Request
    UpdatePhoneNumberSettings (..),
    newUpdatePhoneNumberSettings,

    -- * Request Lenses
    updatePhoneNumberSettings_callingName,

    -- * Destructuring the Response
    UpdatePhoneNumberSettingsResponse (..),
    newUpdatePhoneNumberSettingsResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePhoneNumberSettings' smart constructor.
data UpdatePhoneNumberSettings = UpdatePhoneNumberSettings'
  { callingName :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumberSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'callingName', 'updatePhoneNumberSettings_callingName' - Undocumented member.
newUpdatePhoneNumberSettings ::
  -- | 'callingName'
  Prelude.Text ->
  UpdatePhoneNumberSettings
newUpdatePhoneNumberSettings pCallingName_ =
  UpdatePhoneNumberSettings'
    { callingName =
        Data._Sensitive Lens.# pCallingName_
    }

-- | Undocumented member.
updatePhoneNumberSettings_callingName :: Lens.Lens' UpdatePhoneNumberSettings Prelude.Text
updatePhoneNumberSettings_callingName = Lens.lens (\UpdatePhoneNumberSettings' {callingName} -> callingName) (\s@UpdatePhoneNumberSettings' {} a -> s {callingName = a} :: UpdatePhoneNumberSettings) Prelude.. Data._Sensitive

instance Core.AWSRequest UpdatePhoneNumberSettings where
  type
    AWSResponse UpdatePhoneNumberSettings =
      UpdatePhoneNumberSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdatePhoneNumberSettingsResponse'

instance Prelude.Hashable UpdatePhoneNumberSettings where
  hashWithSalt _salt UpdatePhoneNumberSettings' {..} =
    _salt `Prelude.hashWithSalt` callingName

instance Prelude.NFData UpdatePhoneNumberSettings where
  rnf UpdatePhoneNumberSettings' {..} =
    Prelude.rnf callingName

instance Data.ToHeaders UpdatePhoneNumberSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdatePhoneNumberSettings where
  toJSON UpdatePhoneNumberSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("CallingName" Data..= callingName)]
      )

instance Data.ToPath UpdatePhoneNumberSettings where
  toPath = Prelude.const "/settings/phone-number"

instance Data.ToQuery UpdatePhoneNumberSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePhoneNumberSettingsResponse' smart constructor.
data UpdatePhoneNumberSettingsResponse = UpdatePhoneNumberSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumberSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdatePhoneNumberSettingsResponse ::
  UpdatePhoneNumberSettingsResponse
newUpdatePhoneNumberSettingsResponse =
  UpdatePhoneNumberSettingsResponse'

instance
  Prelude.NFData
    UpdatePhoneNumberSettingsResponse
  where
  rnf _ = ()
