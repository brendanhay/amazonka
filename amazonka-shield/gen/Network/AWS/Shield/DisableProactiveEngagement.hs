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
-- Module      : Network.AWS.Shield.DisableProactiveEngagement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization from the DDoS Response Team (DRT) to notify
-- contacts about escalations to the DRT and to initiate proactive customer
-- support.
module Network.AWS.Shield.DisableProactiveEngagement
  ( -- * Creating a Request
    DisableProactiveEngagement (..),
    newDisableProactiveEngagement,

    -- * Destructuring the Response
    DisableProactiveEngagementResponse (..),
    newDisableProactiveEngagementResponse,

    -- * Response Lenses
    disableProactiveEngagementResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDisableProactiveEngagement' smart constructor.
data DisableProactiveEngagement = DisableProactiveEngagement'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableProactiveEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableProactiveEngagement ::
  DisableProactiveEngagement
newDisableProactiveEngagement =
  DisableProactiveEngagement'

instance Core.AWSRequest DisableProactiveEngagement where
  type
    AWSResponse DisableProactiveEngagement =
      DisableProactiveEngagementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableProactiveEngagementResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisableProactiveEngagement

instance Core.NFData DisableProactiveEngagement

instance Core.ToHeaders DisableProactiveEngagement where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DisableProactiveEngagement" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisableProactiveEngagement where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DisableProactiveEngagement where
  toPath = Core.const "/"

instance Core.ToQuery DisableProactiveEngagement where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisableProactiveEngagementResponse' smart constructor.
data DisableProactiveEngagementResponse = DisableProactiveEngagementResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableProactiveEngagementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableProactiveEngagementResponse_httpStatus' - The response's http status code.
newDisableProactiveEngagementResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableProactiveEngagementResponse
newDisableProactiveEngagementResponse pHttpStatus_ =
  DisableProactiveEngagementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableProactiveEngagementResponse_httpStatus :: Lens.Lens' DisableProactiveEngagementResponse Core.Int
disableProactiveEngagementResponse_httpStatus = Lens.lens (\DisableProactiveEngagementResponse' {httpStatus} -> httpStatus) (\s@DisableProactiveEngagementResponse' {} a -> s {httpStatus = a} :: DisableProactiveEngagementResponse)

instance
  Core.NFData
    DisableProactiveEngagementResponse
