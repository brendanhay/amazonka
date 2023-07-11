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
-- Module      : Amazonka.IoT.SetV2LoggingLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging level.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions SetV2LoggingLevel>
-- action.
module Amazonka.IoT.SetV2LoggingLevel
  ( -- * Creating a Request
    SetV2LoggingLevel (..),
    newSetV2LoggingLevel,

    -- * Request Lenses
    setV2LoggingLevel_logTarget,
    setV2LoggingLevel_logLevel,

    -- * Destructuring the Response
    SetV2LoggingLevelResponse (..),
    newSetV2LoggingLevelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetV2LoggingLevel' smart constructor.
data SetV2LoggingLevel = SetV2LoggingLevel'
  { -- | The log target.
    logTarget :: LogTarget,
    -- | The log level.
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetV2LoggingLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logTarget', 'setV2LoggingLevel_logTarget' - The log target.
--
-- 'logLevel', 'setV2LoggingLevel_logLevel' - The log level.
newSetV2LoggingLevel ::
  -- | 'logTarget'
  LogTarget ->
  -- | 'logLevel'
  LogLevel ->
  SetV2LoggingLevel
newSetV2LoggingLevel pLogTarget_ pLogLevel_ =
  SetV2LoggingLevel'
    { logTarget = pLogTarget_,
      logLevel = pLogLevel_
    }

-- | The log target.
setV2LoggingLevel_logTarget :: Lens.Lens' SetV2LoggingLevel LogTarget
setV2LoggingLevel_logTarget = Lens.lens (\SetV2LoggingLevel' {logTarget} -> logTarget) (\s@SetV2LoggingLevel' {} a -> s {logTarget = a} :: SetV2LoggingLevel)

-- | The log level.
setV2LoggingLevel_logLevel :: Lens.Lens' SetV2LoggingLevel LogLevel
setV2LoggingLevel_logLevel = Lens.lens (\SetV2LoggingLevel' {logLevel} -> logLevel) (\s@SetV2LoggingLevel' {} a -> s {logLevel = a} :: SetV2LoggingLevel)

instance Core.AWSRequest SetV2LoggingLevel where
  type
    AWSResponse SetV2LoggingLevel =
      SetV2LoggingLevelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull SetV2LoggingLevelResponse'

instance Prelude.Hashable SetV2LoggingLevel where
  hashWithSalt _salt SetV2LoggingLevel' {..} =
    _salt
      `Prelude.hashWithSalt` logTarget
      `Prelude.hashWithSalt` logLevel

instance Prelude.NFData SetV2LoggingLevel where
  rnf SetV2LoggingLevel' {..} =
    Prelude.rnf logTarget
      `Prelude.seq` Prelude.rnf logLevel

instance Data.ToHeaders SetV2LoggingLevel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SetV2LoggingLevel where
  toJSON SetV2LoggingLevel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logTarget" Data..= logTarget),
            Prelude.Just ("logLevel" Data..= logLevel)
          ]
      )

instance Data.ToPath SetV2LoggingLevel where
  toPath = Prelude.const "/v2LoggingLevel"

instance Data.ToQuery SetV2LoggingLevel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetV2LoggingLevelResponse' smart constructor.
data SetV2LoggingLevelResponse = SetV2LoggingLevelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetV2LoggingLevelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetV2LoggingLevelResponse ::
  SetV2LoggingLevelResponse
newSetV2LoggingLevelResponse =
  SetV2LoggingLevelResponse'

instance Prelude.NFData SetV2LoggingLevelResponse where
  rnf _ = ()
