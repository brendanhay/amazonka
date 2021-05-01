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
-- Module      : Network.AWS.IoT.SetLoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options.
--
-- NOTE: use of this command is not recommended. Use @SetV2LoggingOptions@
-- instead.
module Network.AWS.IoT.SetLoggingOptions
  ( -- * Creating a Request
    SetLoggingOptions (..),
    newSetLoggingOptions,

    -- * Request Lenses
    setLoggingOptions_loggingOptionsPayload,

    -- * Destructuring the Response
    SetLoggingOptionsResponse (..),
    newSetLoggingOptionsResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetLoggingOptions operation.
--
-- /See:/ 'newSetLoggingOptions' smart constructor.
data SetLoggingOptions = SetLoggingOptions'
  { -- | The logging options payload.
    loggingOptionsPayload :: LoggingOptionsPayload
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingOptionsPayload', 'setLoggingOptions_loggingOptionsPayload' - The logging options payload.
newSetLoggingOptions ::
  -- | 'loggingOptionsPayload'
  LoggingOptionsPayload ->
  SetLoggingOptions
newSetLoggingOptions pLoggingOptionsPayload_ =
  SetLoggingOptions'
    { loggingOptionsPayload =
        pLoggingOptionsPayload_
    }

-- | The logging options payload.
setLoggingOptions_loggingOptionsPayload :: Lens.Lens' SetLoggingOptions LoggingOptionsPayload
setLoggingOptions_loggingOptionsPayload = Lens.lens (\SetLoggingOptions' {loggingOptionsPayload} -> loggingOptionsPayload) (\s@SetLoggingOptions' {} a -> s {loggingOptionsPayload = a} :: SetLoggingOptions)

instance Prelude.AWSRequest SetLoggingOptions where
  type Rs SetLoggingOptions = SetLoggingOptionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull SetLoggingOptionsResponse'

instance Prelude.Hashable SetLoggingOptions

instance Prelude.NFData SetLoggingOptions

instance Prelude.ToHeaders SetLoggingOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON SetLoggingOptions where
  toJSON SetLoggingOptions' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "loggingOptionsPayload"
                  Prelude..= loggingOptionsPayload
              )
          ]
      )

instance Prelude.ToPath SetLoggingOptions where
  toPath = Prelude.const "/loggingOptions"

instance Prelude.ToQuery SetLoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetLoggingOptionsResponse' smart constructor.
data SetLoggingOptionsResponse = SetLoggingOptionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newSetLoggingOptionsResponse ::
  SetLoggingOptionsResponse
newSetLoggingOptionsResponse =
  SetLoggingOptionsResponse'

instance Prelude.NFData SetLoggingOptionsResponse
