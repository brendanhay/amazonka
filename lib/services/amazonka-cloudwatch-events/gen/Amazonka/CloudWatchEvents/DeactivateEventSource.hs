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
-- Module      : Amazonka.CloudWatchEvents.DeactivateEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- To activate a deactivated partner event source, use
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_ActivateEventSource.html ActivateEventSource>.
module Amazonka.CloudWatchEvents.DeactivateEventSource
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

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateEventSource' smart constructor.
data DeactivateEventSource = DeactivateEventSource'
  { -- | The name of the partner event source to deactivate.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeactivateEventSource where
  type
    AWSResponse DeactivateEventSource =
      DeactivateEventSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeactivateEventSourceResponse'

instance Prelude.Hashable DeactivateEventSource where
  hashWithSalt _salt DeactivateEventSource' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeactivateEventSource where
  rnf DeactivateEventSource' {..} = Prelude.rnf name

instance Data.ToHeaders DeactivateEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.DeactivateEventSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeactivateEventSource where
  toJSON DeactivateEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DeactivateEventSource where
  toPath = Prelude.const "/"

instance Data.ToQuery DeactivateEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateEventSourceResponse ::
  DeactivateEventSourceResponse
newDeactivateEventSourceResponse =
  DeactivateEventSourceResponse'

instance Prelude.NFData DeactivateEventSourceResponse where
  rnf _ = ()
