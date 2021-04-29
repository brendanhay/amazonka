{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.SetV2LoggingLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging level.
module Network.AWS.IoT.SetV2LoggingLevel
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetV2LoggingLevel' smart constructor.
data SetV2LoggingLevel = SetV2LoggingLevel'
  { -- | The log target.
    logTarget :: LogTarget,
    -- | The log level.
    logLevel :: LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest SetV2LoggingLevel where
  type Rs SetV2LoggingLevel = SetV2LoggingLevelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetV2LoggingLevelResponse'

instance Prelude.Hashable SetV2LoggingLevel

instance Prelude.NFData SetV2LoggingLevel

instance Prelude.ToHeaders SetV2LoggingLevel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON SetV2LoggingLevel where
  toJSON SetV2LoggingLevel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("logTarget" Prelude..= logTarget),
            Prelude.Just ("logLevel" Prelude..= logLevel)
          ]
      )

instance Prelude.ToPath SetV2LoggingLevel where
  toPath = Prelude.const "/v2LoggingLevel"

instance Prelude.ToQuery SetV2LoggingLevel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetV2LoggingLevelResponse' smart constructor.
data SetV2LoggingLevelResponse = SetV2LoggingLevelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetV2LoggingLevelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetV2LoggingLevelResponse ::
  SetV2LoggingLevelResponse
newSetV2LoggingLevelResponse =
  SetV2LoggingLevelResponse'

instance Prelude.NFData SetV2LoggingLevelResponse
