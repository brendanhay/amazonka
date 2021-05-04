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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetV2LoggingOptions' smart constructor.
data SetV2LoggingOptions = SetV2LoggingOptions'
  { -- | The ARN of the role that allows IoT to write to Cloudwatch logs.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | If true all logs are disabled. The default is false.
    disableAllLogs :: Prelude.Maybe Prelude.Bool,
    -- | The default logging level.
    defaultLogLevel :: Prelude.Maybe LogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { roleArn = Prelude.Nothing,
      disableAllLogs = Prelude.Nothing,
      defaultLogLevel = Prelude.Nothing
    }

-- | The ARN of the role that allows IoT to write to Cloudwatch logs.
setV2LoggingOptions_roleArn :: Lens.Lens' SetV2LoggingOptions (Prelude.Maybe Prelude.Text)
setV2LoggingOptions_roleArn = Lens.lens (\SetV2LoggingOptions' {roleArn} -> roleArn) (\s@SetV2LoggingOptions' {} a -> s {roleArn = a} :: SetV2LoggingOptions)

-- | If true all logs are disabled. The default is false.
setV2LoggingOptions_disableAllLogs :: Lens.Lens' SetV2LoggingOptions (Prelude.Maybe Prelude.Bool)
setV2LoggingOptions_disableAllLogs = Lens.lens (\SetV2LoggingOptions' {disableAllLogs} -> disableAllLogs) (\s@SetV2LoggingOptions' {} a -> s {disableAllLogs = a} :: SetV2LoggingOptions)

-- | The default logging level.
setV2LoggingOptions_defaultLogLevel :: Lens.Lens' SetV2LoggingOptions (Prelude.Maybe LogLevel)
setV2LoggingOptions_defaultLogLevel = Lens.lens (\SetV2LoggingOptions' {defaultLogLevel} -> defaultLogLevel) (\s@SetV2LoggingOptions' {} a -> s {defaultLogLevel = a} :: SetV2LoggingOptions)

instance Prelude.AWSRequest SetV2LoggingOptions where
  type
    Rs SetV2LoggingOptions =
      SetV2LoggingOptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetV2LoggingOptionsResponse'

instance Prelude.Hashable SetV2LoggingOptions

instance Prelude.NFData SetV2LoggingOptions

instance Prelude.ToHeaders SetV2LoggingOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON SetV2LoggingOptions where
  toJSON SetV2LoggingOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("disableAllLogs" Prelude..=)
              Prelude.<$> disableAllLogs,
            ("defaultLogLevel" Prelude..=)
              Prelude.<$> defaultLogLevel
          ]
      )

instance Prelude.ToPath SetV2LoggingOptions where
  toPath = Prelude.const "/v2LoggingOptions"

instance Prelude.ToQuery SetV2LoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetV2LoggingOptionsResponse' smart constructor.
data SetV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetV2LoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetV2LoggingOptionsResponse ::
  SetV2LoggingOptionsResponse
newSetV2LoggingOptionsResponse =
  SetV2LoggingOptionsResponse'

instance Prelude.NFData SetV2LoggingOptionsResponse
