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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeactivateEventSource' smart constructor.
data DeactivateEventSource = DeactivateEventSource'
  { -- | The name of the partner event source to deactivate.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeactivateEventSource
newDeactivateEventSource pName_ =
  DeactivateEventSource' {name = pName_}

-- | The name of the partner event source to deactivate.
deactivateEventSource_name :: Lens.Lens' DeactivateEventSource Prelude.Text
deactivateEventSource_name = Lens.lens (\DeactivateEventSource' {name} -> name) (\s@DeactivateEventSource' {} a -> s {name = a} :: DeactivateEventSource)

instance Prelude.AWSRequest DeactivateEventSource where
  type
    Rs DeactivateEventSource =
      DeactivateEventSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeactivateEventSourceResponse'

instance Prelude.Hashable DeactivateEventSource

instance Prelude.NFData DeactivateEventSource

instance Prelude.ToHeaders DeactivateEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSEvents.DeactivateEventSource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeactivateEventSource where
  toJSON DeactivateEventSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Prelude..= name)]
      )

instance Prelude.ToPath DeactivateEventSource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeactivateEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeactivateEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateEventSourceResponse ::
  DeactivateEventSourceResponse
newDeactivateEventSourceResponse =
  DeactivateEventSourceResponse'

instance Prelude.NFData DeactivateEventSourceResponse
