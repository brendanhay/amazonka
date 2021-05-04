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
-- Module      : Network.AWS.Shield.EnableProactiveEngagement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to use email and phone to notify
-- contacts about escalations to the DRT and to initiate proactive customer
-- support.
module Network.AWS.Shield.EnableProactiveEngagement
  ( -- * Creating a Request
    EnableProactiveEngagement (..),
    newEnableProactiveEngagement,

    -- * Destructuring the Response
    EnableProactiveEngagementResponse (..),
    newEnableProactiveEngagementResponse,

    -- * Response Lenses
    enableProactiveEngagementResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newEnableProactiveEngagement' smart constructor.
data EnableProactiveEngagement = EnableProactiveEngagement'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableProactiveEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableProactiveEngagement ::
  EnableProactiveEngagement
newEnableProactiveEngagement =
  EnableProactiveEngagement'

instance Prelude.AWSRequest EnableProactiveEngagement where
  type
    Rs EnableProactiveEngagement =
      EnableProactiveEngagementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableProactiveEngagementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableProactiveEngagement

instance Prelude.NFData EnableProactiveEngagement

instance Prelude.ToHeaders EnableProactiveEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.EnableProactiveEngagement" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON EnableProactiveEngagement where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath EnableProactiveEngagement where
  toPath = Prelude.const "/"

instance Prelude.ToQuery EnableProactiveEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableProactiveEngagementResponse' smart constructor.
data EnableProactiveEngagementResponse = EnableProactiveEngagementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableProactiveEngagementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableProactiveEngagementResponse_httpStatus' - The response's http status code.
newEnableProactiveEngagementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableProactiveEngagementResponse
newEnableProactiveEngagementResponse pHttpStatus_ =
  EnableProactiveEngagementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
enableProactiveEngagementResponse_httpStatus :: Lens.Lens' EnableProactiveEngagementResponse Prelude.Int
enableProactiveEngagementResponse_httpStatus = Lens.lens (\EnableProactiveEngagementResponse' {httpStatus} -> httpStatus) (\s@EnableProactiveEngagementResponse' {} a -> s {httpStatus = a} :: EnableProactiveEngagementResponse)

instance
  Prelude.NFData
    EnableProactiveEngagementResponse
