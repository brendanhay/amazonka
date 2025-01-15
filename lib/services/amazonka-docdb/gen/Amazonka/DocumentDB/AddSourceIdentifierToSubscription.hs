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
-- Module      : Amazonka.DocumentDB.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a source identifier to an existing event notification subscription.
module Amazonka.DocumentDB.AddSourceIdentifierToSubscription
  ( -- * Creating a Request
    AddSourceIdentifierToSubscription (..),
    newAddSourceIdentifierToSubscription,

    -- * Request Lenses
    addSourceIdentifierToSubscription_subscriptionName,
    addSourceIdentifierToSubscription_sourceIdentifier,

    -- * Destructuring the Response
    AddSourceIdentifierToSubscriptionResponse (..),
    newAddSourceIdentifierToSubscriptionResponse,

    -- * Response Lenses
    addSourceIdentifierToSubscriptionResponse_eventSubscription,
    addSourceIdentifierToSubscriptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to AddSourceIdentifierToSubscription.
--
-- /See:/ 'newAddSourceIdentifierToSubscription' smart constructor.
data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription'
  { -- | The name of the Amazon DocumentDB event notification subscription that
    -- you want to add a source identifier to.
    subscriptionName :: Prelude.Text,
    -- | The identifier of the event source to be added:
    --
    -- -   If the source type is an instance, a @DBInstanceIdentifier@ must be
    --     provided.
    --
    -- -   If the source type is a security group, a @DBSecurityGroupName@ must
    --     be provided.
    --
    -- -   If the source type is a parameter group, a @DBParameterGroupName@
    --     must be provided.
    --
    -- -   If the source type is a snapshot, a @DBSnapshotIdentifier@ must be
    --     provided.
    sourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddSourceIdentifierToSubscription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionName', 'addSourceIdentifierToSubscription_subscriptionName' - The name of the Amazon DocumentDB event notification subscription that
-- you want to add a source identifier to.
--
-- 'sourceIdentifier', 'addSourceIdentifierToSubscription_sourceIdentifier' - The identifier of the event source to be added:
--
-- -   If the source type is an instance, a @DBInstanceIdentifier@ must be
--     provided.
--
-- -   If the source type is a security group, a @DBSecurityGroupName@ must
--     be provided.
--
-- -   If the source type is a parameter group, a @DBParameterGroupName@
--     must be provided.
--
-- -   If the source type is a snapshot, a @DBSnapshotIdentifier@ must be
--     provided.
newAddSourceIdentifierToSubscription ::
  -- | 'subscriptionName'
  Prelude.Text ->
  -- | 'sourceIdentifier'
  Prelude.Text ->
  AddSourceIdentifierToSubscription
newAddSourceIdentifierToSubscription
  pSubscriptionName_
  pSourceIdentifier_ =
    AddSourceIdentifierToSubscription'
      { subscriptionName =
          pSubscriptionName_,
        sourceIdentifier = pSourceIdentifier_
      }

-- | The name of the Amazon DocumentDB event notification subscription that
-- you want to add a source identifier to.
addSourceIdentifierToSubscription_subscriptionName :: Lens.Lens' AddSourceIdentifierToSubscription Prelude.Text
addSourceIdentifierToSubscription_subscriptionName = Lens.lens (\AddSourceIdentifierToSubscription' {subscriptionName} -> subscriptionName) (\s@AddSourceIdentifierToSubscription' {} a -> s {subscriptionName = a} :: AddSourceIdentifierToSubscription)

-- | The identifier of the event source to be added:
--
-- -   If the source type is an instance, a @DBInstanceIdentifier@ must be
--     provided.
--
-- -   If the source type is a security group, a @DBSecurityGroupName@ must
--     be provided.
--
-- -   If the source type is a parameter group, a @DBParameterGroupName@
--     must be provided.
--
-- -   If the source type is a snapshot, a @DBSnapshotIdentifier@ must be
--     provided.
addSourceIdentifierToSubscription_sourceIdentifier :: Lens.Lens' AddSourceIdentifierToSubscription Prelude.Text
addSourceIdentifierToSubscription_sourceIdentifier = Lens.lens (\AddSourceIdentifierToSubscription' {sourceIdentifier} -> sourceIdentifier) (\s@AddSourceIdentifierToSubscription' {} a -> s {sourceIdentifier = a} :: AddSourceIdentifierToSubscription)

instance
  Core.AWSRequest
    AddSourceIdentifierToSubscription
  where
  type
    AWSResponse AddSourceIdentifierToSubscription =
      AddSourceIdentifierToSubscriptionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "AddSourceIdentifierToSubscriptionResult"
      ( \s h x ->
          AddSourceIdentifierToSubscriptionResponse'
            Prelude.<$> (x Data..@? "EventSubscription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AddSourceIdentifierToSubscription
  where
  hashWithSalt
    _salt
    AddSourceIdentifierToSubscription' {..} =
      _salt
        `Prelude.hashWithSalt` subscriptionName
        `Prelude.hashWithSalt` sourceIdentifier

instance
  Prelude.NFData
    AddSourceIdentifierToSubscription
  where
  rnf AddSourceIdentifierToSubscription' {..} =
    Prelude.rnf subscriptionName `Prelude.seq`
      Prelude.rnf sourceIdentifier

instance
  Data.ToHeaders
    AddSourceIdentifierToSubscription
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    AddSourceIdentifierToSubscription
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AddSourceIdentifierToSubscription
  where
  toQuery AddSourceIdentifierToSubscription' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "AddSourceIdentifierToSubscription" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "SubscriptionName" Data.=: subscriptionName,
        "SourceIdentifier" Data.=: sourceIdentifier
      ]

-- | /See:/ 'newAddSourceIdentifierToSubscriptionResponse' smart constructor.
data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse'
  { eventSubscription :: Prelude.Maybe EventSubscription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddSourceIdentifierToSubscriptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSubscription', 'addSourceIdentifierToSubscriptionResponse_eventSubscription' - Undocumented member.
--
-- 'httpStatus', 'addSourceIdentifierToSubscriptionResponse_httpStatus' - The response's http status code.
newAddSourceIdentifierToSubscriptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddSourceIdentifierToSubscriptionResponse
newAddSourceIdentifierToSubscriptionResponse
  pHttpStatus_ =
    AddSourceIdentifierToSubscriptionResponse'
      { eventSubscription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
addSourceIdentifierToSubscriptionResponse_eventSubscription :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse (Prelude.Maybe EventSubscription)
addSourceIdentifierToSubscriptionResponse_eventSubscription = Lens.lens (\AddSourceIdentifierToSubscriptionResponse' {eventSubscription} -> eventSubscription) (\s@AddSourceIdentifierToSubscriptionResponse' {} a -> s {eventSubscription = a} :: AddSourceIdentifierToSubscriptionResponse)

-- | The response's http status code.
addSourceIdentifierToSubscriptionResponse_httpStatus :: Lens.Lens' AddSourceIdentifierToSubscriptionResponse Prelude.Int
addSourceIdentifierToSubscriptionResponse_httpStatus = Lens.lens (\AddSourceIdentifierToSubscriptionResponse' {httpStatus} -> httpStatus) (\s@AddSourceIdentifierToSubscriptionResponse' {} a -> s {httpStatus = a} :: AddSourceIdentifierToSubscriptionResponse)

instance
  Prelude.NFData
    AddSourceIdentifierToSubscriptionResponse
  where
  rnf AddSourceIdentifierToSubscriptionResponse' {..} =
    Prelude.rnf eventSubscription `Prelude.seq`
      Prelude.rnf httpStatus
