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
-- Module      : Network.AWS.IoTAnalytics.PutLoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or updates the AWS IoT Analytics logging options.
--
-- If you update the value of any @loggingOptions@ field, it takes up to
-- one minute for the change to take effect. Also, if you change the policy
-- attached to the role you specified in the @roleArn@ field (for example,
-- to correct an invalid policy), it takes up to five minutes for that
-- change to take effect.
module Network.AWS.IoTAnalytics.PutLoggingOptions
  ( -- * Creating a Request
    PutLoggingOptions (..),
    newPutLoggingOptions,

    -- * Request Lenses
    putLoggingOptions_loggingOptions,

    -- * Destructuring the Response
    PutLoggingOptionsResponse (..),
    newPutLoggingOptionsResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLoggingOptions' smart constructor.
data PutLoggingOptions = PutLoggingOptions'
  { -- | The new values of the AWS IoT Analytics logging options.
    loggingOptions :: LoggingOptions
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingOptions', 'putLoggingOptions_loggingOptions' - The new values of the AWS IoT Analytics logging options.
newPutLoggingOptions ::
  -- | 'loggingOptions'
  LoggingOptions ->
  PutLoggingOptions
newPutLoggingOptions pLoggingOptions_ =
  PutLoggingOptions'
    { loggingOptions =
        pLoggingOptions_
    }

-- | The new values of the AWS IoT Analytics logging options.
putLoggingOptions_loggingOptions :: Lens.Lens' PutLoggingOptions LoggingOptions
putLoggingOptions_loggingOptions = Lens.lens (\PutLoggingOptions' {loggingOptions} -> loggingOptions) (\s@PutLoggingOptions' {} a -> s {loggingOptions = a} :: PutLoggingOptions)

instance Core.AWSRequest PutLoggingOptions where
  type
    AWSResponse PutLoggingOptions =
      PutLoggingOptionsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull PutLoggingOptionsResponse'

instance Core.Hashable PutLoggingOptions

instance Core.NFData PutLoggingOptions

instance Core.ToHeaders PutLoggingOptions where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PutLoggingOptions where
  toJSON PutLoggingOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loggingOptions" Core..= loggingOptions)
          ]
      )

instance Core.ToPath PutLoggingOptions where
  toPath = Core.const "/logging"

instance Core.ToQuery PutLoggingOptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutLoggingOptionsResponse' smart constructor.
data PutLoggingOptionsResponse = PutLoggingOptionsResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutLoggingOptionsResponse ::
  PutLoggingOptionsResponse
newPutLoggingOptionsResponse =
  PutLoggingOptionsResponse'

instance Core.NFData PutLoggingOptionsResponse
