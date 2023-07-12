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
-- Module      : Amazonka.CloudWatchEvents.CreatePartnerEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Called by an SaaS partner to create a partner event source. This
-- operation is not used by Amazon Web Services customers.
--
-- Each partner event source can be used by one Amazon Web Services account
-- to create a matching partner event bus in that Amazon Web Services
-- account. A SaaS partner must create one partner event source for each
-- Amazon Web Services account that wants to receive those event types.
--
-- A partner event source creates events based on resources within the SaaS
-- partner\'s service or application.
--
-- An Amazon Web Services account that creates a partner event bus that
-- matches the partner event source can use that event bus to receive
-- events from the partner, and then process them using Amazon Web Services
-- Events rules and targets.
--
-- Partner event source names follow this format:
--
-- @ @/@partner_name@/@\/@/@event_namespace@/@\/@/@event_name@/@ @
--
-- /partner_name/ is determined during partner registration and identifies
-- the partner to Amazon Web Services customers. /event_namespace/ is
-- determined by the partner and is a way for the partner to categorize
-- their events. /event_name/ is determined by the partner, and should
-- uniquely identify an event-generating resource within the partner
-- system. The combination of /event_namespace/ and /event_name/ should
-- help Amazon Web Services customers decide whether to create an event bus
-- to receive these events.
module Amazonka.CloudWatchEvents.CreatePartnerEventSource
  ( -- * Creating a Request
    CreatePartnerEventSource (..),
    newCreatePartnerEventSource,

    -- * Request Lenses
    createPartnerEventSource_name,
    createPartnerEventSource_account,

    -- * Destructuring the Response
    CreatePartnerEventSourceResponse (..),
    newCreatePartnerEventSourceResponse,

    -- * Response Lenses
    createPartnerEventSourceResponse_eventSourceArn,
    createPartnerEventSourceResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePartnerEventSource' smart constructor.
data CreatePartnerEventSource = CreatePartnerEventSource'
  { -- | The name of the partner event source. This name must be unique and must
    -- be in the format
    -- @ @/@partner_name@/@\/@/@event_namespace@/@\/@/@event_name@/@ @. The
    -- Amazon Web Services account that wants to use this partner event source
    -- must create a partner event bus with a name that matches the name of the
    -- partner event source.
    name :: Prelude.Text,
    -- | The Amazon Web Services account ID that is permitted to create a
    -- matching partner event bus for this partner event source.
    account :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePartnerEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createPartnerEventSource_name' - The name of the partner event source. This name must be unique and must
-- be in the format
-- @ @/@partner_name@/@\/@/@event_namespace@/@\/@/@event_name@/@ @. The
-- Amazon Web Services account that wants to use this partner event source
-- must create a partner event bus with a name that matches the name of the
-- partner event source.
--
-- 'account', 'createPartnerEventSource_account' - The Amazon Web Services account ID that is permitted to create a
-- matching partner event bus for this partner event source.
newCreatePartnerEventSource ::
  -- | 'name'
  Prelude.Text ->
  -- | 'account'
  Prelude.Text ->
  CreatePartnerEventSource
newCreatePartnerEventSource pName_ pAccount_ =
  CreatePartnerEventSource'
    { name = pName_,
      account = pAccount_
    }

-- | The name of the partner event source. This name must be unique and must
-- be in the format
-- @ @/@partner_name@/@\/@/@event_namespace@/@\/@/@event_name@/@ @. The
-- Amazon Web Services account that wants to use this partner event source
-- must create a partner event bus with a name that matches the name of the
-- partner event source.
createPartnerEventSource_name :: Lens.Lens' CreatePartnerEventSource Prelude.Text
createPartnerEventSource_name = Lens.lens (\CreatePartnerEventSource' {name} -> name) (\s@CreatePartnerEventSource' {} a -> s {name = a} :: CreatePartnerEventSource)

-- | The Amazon Web Services account ID that is permitted to create a
-- matching partner event bus for this partner event source.
createPartnerEventSource_account :: Lens.Lens' CreatePartnerEventSource Prelude.Text
createPartnerEventSource_account = Lens.lens (\CreatePartnerEventSource' {account} -> account) (\s@CreatePartnerEventSource' {} a -> s {account = a} :: CreatePartnerEventSource)

instance Core.AWSRequest CreatePartnerEventSource where
  type
    AWSResponse CreatePartnerEventSource =
      CreatePartnerEventSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePartnerEventSourceResponse'
            Prelude.<$> (x Data..?> "EventSourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePartnerEventSource where
  hashWithSalt _salt CreatePartnerEventSource' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` account

instance Prelude.NFData CreatePartnerEventSource where
  rnf CreatePartnerEventSource' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf account

instance Data.ToHeaders CreatePartnerEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.CreatePartnerEventSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePartnerEventSource where
  toJSON CreatePartnerEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Account" Data..= account)
          ]
      )

instance Data.ToPath CreatePartnerEventSource where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePartnerEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePartnerEventSourceResponse' smart constructor.
data CreatePartnerEventSourceResponse = CreatePartnerEventSourceResponse'
  { -- | The ARN of the partner event source.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePartnerEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'createPartnerEventSourceResponse_eventSourceArn' - The ARN of the partner event source.
--
-- 'httpStatus', 'createPartnerEventSourceResponse_httpStatus' - The response's http status code.
newCreatePartnerEventSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePartnerEventSourceResponse
newCreatePartnerEventSourceResponse pHttpStatus_ =
  CreatePartnerEventSourceResponse'
    { eventSourceArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the partner event source.
createPartnerEventSourceResponse_eventSourceArn :: Lens.Lens' CreatePartnerEventSourceResponse (Prelude.Maybe Prelude.Text)
createPartnerEventSourceResponse_eventSourceArn = Lens.lens (\CreatePartnerEventSourceResponse' {eventSourceArn} -> eventSourceArn) (\s@CreatePartnerEventSourceResponse' {} a -> s {eventSourceArn = a} :: CreatePartnerEventSourceResponse)

-- | The response's http status code.
createPartnerEventSourceResponse_httpStatus :: Lens.Lens' CreatePartnerEventSourceResponse Prelude.Int
createPartnerEventSourceResponse_httpStatus = Lens.lens (\CreatePartnerEventSourceResponse' {httpStatus} -> httpStatus) (\s@CreatePartnerEventSourceResponse' {} a -> s {httpStatus = a} :: CreatePartnerEventSourceResponse)

instance
  Prelude.NFData
    CreatePartnerEventSourceResponse
  where
  rnf CreatePartnerEventSourceResponse' {..} =
    Prelude.rnf eventSourceArn
      `Prelude.seq` Prelude.rnf httpStatus
