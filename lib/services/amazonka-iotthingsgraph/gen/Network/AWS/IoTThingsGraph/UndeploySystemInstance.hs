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
-- Module      : Network.AWS.IoTThingsGraph.UndeploySystemInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a system instance from its target (Cloud or Greengrass).
module Network.AWS.IoTThingsGraph.UndeploySystemInstance
  ( -- * Creating a Request
    UndeploySystemInstance (..),
    newUndeploySystemInstance,

    -- * Request Lenses
    undeploySystemInstance_id,

    -- * Destructuring the Response
    UndeploySystemInstanceResponse (..),
    newUndeploySystemInstanceResponse,

    -- * Response Lenses
    undeploySystemInstanceResponse_summary,
    undeploySystemInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTThingsGraph.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUndeploySystemInstance' smart constructor.
data UndeploySystemInstance = UndeploySystemInstance'
  { -- | The ID of the system instance to remove from its target.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UndeploySystemInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'undeploySystemInstance_id' - The ID of the system instance to remove from its target.
newUndeploySystemInstance ::
  UndeploySystemInstance
newUndeploySystemInstance =
  UndeploySystemInstance' {id = Prelude.Nothing}

-- | The ID of the system instance to remove from its target.
undeploySystemInstance_id :: Lens.Lens' UndeploySystemInstance (Prelude.Maybe Prelude.Text)
undeploySystemInstance_id = Lens.lens (\UndeploySystemInstance' {id} -> id) (\s@UndeploySystemInstance' {} a -> s {id = a} :: UndeploySystemInstance)

instance Core.AWSRequest UndeploySystemInstance where
  type
    AWSResponse UndeploySystemInstance =
      UndeploySystemInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UndeploySystemInstanceResponse'
            Prelude.<$> (x Core..?> "summary")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UndeploySystemInstance

instance Prelude.NFData UndeploySystemInstance

instance Core.ToHeaders UndeploySystemInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "IotThingsGraphFrontEndService.UndeploySystemInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UndeploySystemInstance where
  toJSON UndeploySystemInstance' {..} =
    Core.object
      (Prelude.catMaybes [("id" Core..=) Prelude.<$> id])

instance Core.ToPath UndeploySystemInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery UndeploySystemInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUndeploySystemInstanceResponse' smart constructor.
data UndeploySystemInstanceResponse = UndeploySystemInstanceResponse'
  { -- | An object that contains summary information about the system instance
    -- that was removed from its target.
    summary :: Prelude.Maybe SystemInstanceSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UndeploySystemInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'undeploySystemInstanceResponse_summary' - An object that contains summary information about the system instance
-- that was removed from its target.
--
-- 'httpStatus', 'undeploySystemInstanceResponse_httpStatus' - The response's http status code.
newUndeploySystemInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UndeploySystemInstanceResponse
newUndeploySystemInstanceResponse pHttpStatus_ =
  UndeploySystemInstanceResponse'
    { summary =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains summary information about the system instance
-- that was removed from its target.
undeploySystemInstanceResponse_summary :: Lens.Lens' UndeploySystemInstanceResponse (Prelude.Maybe SystemInstanceSummary)
undeploySystemInstanceResponse_summary = Lens.lens (\UndeploySystemInstanceResponse' {summary} -> summary) (\s@UndeploySystemInstanceResponse' {} a -> s {summary = a} :: UndeploySystemInstanceResponse)

-- | The response's http status code.
undeploySystemInstanceResponse_httpStatus :: Lens.Lens' UndeploySystemInstanceResponse Prelude.Int
undeploySystemInstanceResponse_httpStatus = Lens.lens (\UndeploySystemInstanceResponse' {httpStatus} -> httpStatus) (\s@UndeploySystemInstanceResponse' {} a -> s {httpStatus = a} :: UndeploySystemInstanceResponse)

instance
  Prelude.NFData
    UndeploySystemInstanceResponse
