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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDisableProactiveEngagement' smart constructor.
data DisableProactiveEngagement = DisableProactiveEngagement'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableProactiveEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisableProactiveEngagement ::
  DisableProactiveEngagement
newDisableProactiveEngagement =
  DisableProactiveEngagement'

instance
  Prelude.AWSRequest
    DisableProactiveEngagement
  where
  type
    Rs DisableProactiveEngagement =
      DisableProactiveEngagementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableProactiveEngagementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableProactiveEngagement

instance Prelude.NFData DisableProactiveEngagement

instance Prelude.ToHeaders DisableProactiveEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.DisableProactiveEngagement" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisableProactiveEngagement where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DisableProactiveEngagement where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisableProactiveEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableProactiveEngagementResponse' smart constructor.
data DisableProactiveEngagementResponse = DisableProactiveEngagementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DisableProactiveEngagementResponse
newDisableProactiveEngagementResponse pHttpStatus_ =
  DisableProactiveEngagementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disableProactiveEngagementResponse_httpStatus :: Lens.Lens' DisableProactiveEngagementResponse Prelude.Int
disableProactiveEngagementResponse_httpStatus = Lens.lens (\DisableProactiveEngagementResponse' {httpStatus} -> httpStatus) (\s@DisableProactiveEngagementResponse' {} a -> s {httpStatus = a} :: DisableProactiveEngagementResponse)

instance
  Prelude.NFData
    DisableProactiveEngagementResponse
