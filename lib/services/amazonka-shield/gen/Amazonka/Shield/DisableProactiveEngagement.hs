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
-- Module      : Amazonka.Shield.DisableProactiveEngagement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization from the Shield Response Team (SRT) to notify
-- contacts about escalations to the SRT and to initiate proactive customer
-- support.
module Amazonka.Shield.DisableProactiveEngagement
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDisableProactiveEngagement' smart constructor.
data DisableProactiveEngagement = DisableProactiveEngagement'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableProactiveEngagementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableProactiveEngagement where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DisableProactiveEngagement where
  rnf _ = ()

instance Data.ToHeaders DisableProactiveEngagement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DisableProactiveEngagement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableProactiveEngagement where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DisableProactiveEngagement where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableProactiveEngagement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableProactiveEngagementResponse' smart constructor.
data DisableProactiveEngagementResponse = DisableProactiveEngagementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DisableProactiveEngagementResponse' {..} =
    Prelude.rnf httpStatus
