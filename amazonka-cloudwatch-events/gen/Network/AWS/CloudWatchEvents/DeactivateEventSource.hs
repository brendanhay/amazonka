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
-- Module      : Network.AWS.CloudWatchEvents.DeactivateEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this operation to temporarily stop receiving events from the
-- specified partner event source. The matching event bus is not deleted.
--
-- When you deactivate a partner event source, the source goes into PENDING
-- state. If it remains in PENDING state for more than two weeks, it is
-- deleted.
--
-- To activate a deactivated partner event source, use ActivateEventSource.
module Network.AWS.CloudWatchEvents.DeactivateEventSource
  ( -- * Creating a Request
    DeactivateEventSource (..),
    newDeactivateEventSource,

    -- * Request Lenses
    deactivateEventSource_name,

    -- * Destructuring the Response
    DeactivateEventSourceResponse (..),
    newDeactivateEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeactivateEventSource' smart constructor.
data DeactivateEventSource = DeactivateEventSource'
  { -- | The name of the partner event source to deactivate.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeactivateEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deactivateEventSource_name' - The name of the partner event source to deactivate.
newDeactivateEventSource ::
  -- | 'name'
  Core.Text ->
  DeactivateEventSource
newDeactivateEventSource pName_ =
  DeactivateEventSource' {name = pName_}

-- | The name of the partner event source to deactivate.
deactivateEventSource_name :: Lens.Lens' DeactivateEventSource Core.Text
deactivateEventSource_name = Lens.lens (\DeactivateEventSource' {name} -> name) (\s@DeactivateEventSource' {} a -> s {name = a} :: DeactivateEventSource)

instance Core.AWSRequest DeactivateEventSource where
  type
    AWSResponse DeactivateEventSource =
      DeactivateEventSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeactivateEventSourceResponse'

instance Core.Hashable DeactivateEventSource

instance Core.NFData DeactivateEventSource

instance Core.ToHeaders DeactivateEventSource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.DeactivateEventSource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeactivateEventSource where
  toJSON DeactivateEventSource' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DeactivateEventSource where
  toPath = Core.const "/"

instance Core.ToQuery DeactivateEventSource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeactivateEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateEventSourceResponse ::
  DeactivateEventSourceResponse
newDeactivateEventSourceResponse =
  DeactivateEventSourceResponse'

instance Core.NFData DeactivateEventSourceResponse
