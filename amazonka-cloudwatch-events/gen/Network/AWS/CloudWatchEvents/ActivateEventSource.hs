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
-- Module      : Network.AWS.CloudWatchEvents.ActivateEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a partner event source that has been deactivated. Once
-- activated, your matching event bus will start receiving events from the
-- event source.
module Network.AWS.CloudWatchEvents.ActivateEventSource
  ( -- * Creating a Request
    ActivateEventSource (..),
    newActivateEventSource,

    -- * Request Lenses
    activateEventSource_name,

    -- * Destructuring the Response
    ActivateEventSourceResponse (..),
    newActivateEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newActivateEventSource' smart constructor.
data ActivateEventSource = ActivateEventSource'
  { -- | The name of the partner event source to activate.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivateEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'activateEventSource_name' - The name of the partner event source to activate.
newActivateEventSource ::
  -- | 'name'
  Core.Text ->
  ActivateEventSource
newActivateEventSource pName_ =
  ActivateEventSource' {name = pName_}

-- | The name of the partner event source to activate.
activateEventSource_name :: Lens.Lens' ActivateEventSource Core.Text
activateEventSource_name = Lens.lens (\ActivateEventSource' {name} -> name) (\s@ActivateEventSource' {} a -> s {name = a} :: ActivateEventSource)

instance Core.AWSRequest ActivateEventSource where
  type
    AWSResponse ActivateEventSource =
      ActivateEventSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ActivateEventSourceResponse'

instance Core.Hashable ActivateEventSource

instance Core.NFData ActivateEventSource

instance Core.ToHeaders ActivateEventSource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.ActivateEventSource" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ActivateEventSource where
  toJSON ActivateEventSource' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath ActivateEventSource where
  toPath = Core.const "/"

instance Core.ToQuery ActivateEventSource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newActivateEventSourceResponse' smart constructor.
data ActivateEventSourceResponse = ActivateEventSourceResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActivateEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newActivateEventSourceResponse ::
  ActivateEventSourceResponse
newActivateEventSourceResponse =
  ActivateEventSourceResponse'

instance Core.NFData ActivateEventSourceResponse
