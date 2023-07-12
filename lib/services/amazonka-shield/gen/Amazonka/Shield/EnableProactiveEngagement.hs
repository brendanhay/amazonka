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
-- Module      : Amazonka.Shield.EnableProactiveEngagement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the Shield Response Team (SRT) to use email and phone to
-- notify contacts about escalations to the SRT and to initiate proactive
-- customer support.
module Amazonka.Shield.EnableProactiveEngagement
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newEnableProactiveEngagement' smart constructor.
data EnableProactiveEngagement = EnableProactiveEngagement'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableProactiveEngagement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newEnableProactiveEngagement ::
  EnableProactiveEngagement
newEnableProactiveEngagement =
  EnableProactiveEngagement'

instance Core.AWSRequest EnableProactiveEngagement where
  type
    AWSResponse EnableProactiveEngagement =
      EnableProactiveEngagementResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          EnableProactiveEngagementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableProactiveEngagement where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData EnableProactiveEngagement where
  rnf _ = ()

instance Data.ToHeaders EnableProactiveEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.EnableProactiveEngagement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableProactiveEngagement where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath EnableProactiveEngagement where
  toPath = Prelude.const "/"

instance Data.ToQuery EnableProactiveEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableProactiveEngagementResponse' smart constructor.
data EnableProactiveEngagementResponse = EnableProactiveEngagementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf EnableProactiveEngagementResponse' {..} =
    Prelude.rnf httpStatus
