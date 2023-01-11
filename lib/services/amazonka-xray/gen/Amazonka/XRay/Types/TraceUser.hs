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
-- Module      : Amazonka.XRay.Types.TraceUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.TraceUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.ServiceId

-- | Information about a user recorded in segment documents.
--
-- /See:/ 'newTraceUser' smart constructor.
data TraceUser = TraceUser'
  { -- | Services that the user\'s request hit.
    serviceIds :: Prelude.Maybe [ServiceId],
    -- | The user\'s name.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TraceUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIds', 'traceUser_serviceIds' - Services that the user\'s request hit.
--
-- 'userName', 'traceUser_userName' - The user\'s name.
newTraceUser ::
  TraceUser
newTraceUser =
  TraceUser'
    { serviceIds = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | Services that the user\'s request hit.
traceUser_serviceIds :: Lens.Lens' TraceUser (Prelude.Maybe [ServiceId])
traceUser_serviceIds = Lens.lens (\TraceUser' {serviceIds} -> serviceIds) (\s@TraceUser' {} a -> s {serviceIds = a} :: TraceUser) Prelude.. Lens.mapping Lens.coerced

-- | The user\'s name.
traceUser_userName :: Lens.Lens' TraceUser (Prelude.Maybe Prelude.Text)
traceUser_userName = Lens.lens (\TraceUser' {userName} -> userName) (\s@TraceUser' {} a -> s {userName = a} :: TraceUser)

instance Data.FromJSON TraceUser where
  parseJSON =
    Data.withObject
      "TraceUser"
      ( \x ->
          TraceUser'
            Prelude.<$> (x Data..:? "ServiceIds" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "UserName")
      )

instance Prelude.Hashable TraceUser where
  hashWithSalt _salt TraceUser' {..} =
    _salt `Prelude.hashWithSalt` serviceIds
      `Prelude.hashWithSalt` userName

instance Prelude.NFData TraceUser where
  rnf TraceUser' {..} =
    Prelude.rnf serviceIds
      `Prelude.seq` Prelude.rnf userName
