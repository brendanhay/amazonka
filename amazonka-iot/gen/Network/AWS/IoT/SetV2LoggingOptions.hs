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
-- Module      : Network.AWS.IoT.SetV2LoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options for the V2 logging service.
module Network.AWS.IoT.SetV2LoggingOptions
  ( -- * Creating a Request
    SetV2LoggingOptions (..),
    newSetV2LoggingOptions,

    -- * Request Lenses
    setV2LoggingOptions_roleArn,
    setV2LoggingOptions_disableAllLogs,
    setV2LoggingOptions_defaultLogLevel,

    -- * Destructuring the Response
    SetV2LoggingOptionsResponse (..),
    newSetV2LoggingOptionsResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetV2LoggingOptions' smart constructor.
data SetV2LoggingOptions = SetV2LoggingOptions'
  { -- | The ARN of the role that allows IoT to write to Cloudwatch logs.
    roleArn :: Core.Maybe Core.Text,
    -- | If true all logs are disabled. The default is false.
    disableAllLogs :: Core.Maybe Core.Bool,
    -- | The default logging level.
    defaultLogLevel :: Core.Maybe LogLevel
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetV2LoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'setV2LoggingOptions_roleArn' - The ARN of the role that allows IoT to write to Cloudwatch logs.
--
-- 'disableAllLogs', 'setV2LoggingOptions_disableAllLogs' - If true all logs are disabled. The default is false.
--
-- 'defaultLogLevel', 'setV2LoggingOptions_defaultLogLevel' - The default logging level.
newSetV2LoggingOptions ::
  SetV2LoggingOptions
newSetV2LoggingOptions =
  SetV2LoggingOptions'
    { roleArn = Core.Nothing,
      disableAllLogs = Core.Nothing,
      defaultLogLevel = Core.Nothing
    }

-- | The ARN of the role that allows IoT to write to Cloudwatch logs.
setV2LoggingOptions_roleArn :: Lens.Lens' SetV2LoggingOptions (Core.Maybe Core.Text)
setV2LoggingOptions_roleArn = Lens.lens (\SetV2LoggingOptions' {roleArn} -> roleArn) (\s@SetV2LoggingOptions' {} a -> s {roleArn = a} :: SetV2LoggingOptions)

-- | If true all logs are disabled. The default is false.
setV2LoggingOptions_disableAllLogs :: Lens.Lens' SetV2LoggingOptions (Core.Maybe Core.Bool)
setV2LoggingOptions_disableAllLogs = Lens.lens (\SetV2LoggingOptions' {disableAllLogs} -> disableAllLogs) (\s@SetV2LoggingOptions' {} a -> s {disableAllLogs = a} :: SetV2LoggingOptions)

-- | The default logging level.
setV2LoggingOptions_defaultLogLevel :: Lens.Lens' SetV2LoggingOptions (Core.Maybe LogLevel)
setV2LoggingOptions_defaultLogLevel = Lens.lens (\SetV2LoggingOptions' {defaultLogLevel} -> defaultLogLevel) (\s@SetV2LoggingOptions' {} a -> s {defaultLogLevel = a} :: SetV2LoggingOptions)

instance Core.AWSRequest SetV2LoggingOptions where
  type
    AWSResponse SetV2LoggingOptions =
      SetV2LoggingOptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetV2LoggingOptionsResponse'

instance Core.Hashable SetV2LoggingOptions

instance Core.NFData SetV2LoggingOptions

instance Core.ToHeaders SetV2LoggingOptions where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON SetV2LoggingOptions where
  toJSON SetV2LoggingOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("roleArn" Core..=) Core.<$> roleArn,
            ("disableAllLogs" Core..=) Core.<$> disableAllLogs,
            ("defaultLogLevel" Core..=)
              Core.<$> defaultLogLevel
          ]
      )

instance Core.ToPath SetV2LoggingOptions where
  toPath = Core.const "/v2LoggingOptions"

instance Core.ToQuery SetV2LoggingOptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetV2LoggingOptionsResponse' smart constructor.
data SetV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetV2LoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetV2LoggingOptionsResponse ::
  SetV2LoggingOptionsResponse
newSetV2LoggingOptionsResponse =
  SetV2LoggingOptionsResponse'

instance Core.NFData SetV2LoggingOptionsResponse
