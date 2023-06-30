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
-- Module      : Amazonka.CloudWatchLogs.PutDestinationPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an access policy associated with an existing
-- destination. An access policy is an
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies_overview.html IAM policy document>
-- that is used to authorize claims to register a subscription filter
-- against a given destination.
module Amazonka.CloudWatchLogs.PutDestinationPolicy
  ( -- * Creating a Request
    PutDestinationPolicy (..),
    newPutDestinationPolicy,

    -- * Request Lenses
    putDestinationPolicy_forceUpdate,
    putDestinationPolicy_destinationName,
    putDestinationPolicy_accessPolicy,

    -- * Destructuring the Response
    PutDestinationPolicyResponse (..),
    newPutDestinationPolicyResponse,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDestinationPolicy' smart constructor.
data PutDestinationPolicy = PutDestinationPolicy'
  { -- | Specify true if you are updating an existing destination policy to grant
    -- permission to an organization ID instead of granting permission to
    -- individual AWS accounts. Before you update a destination policy this
    -- way, you must first update the subscription filters in the accounts that
    -- send logs to this destination. If you do not, the subscription filters
    -- might stop working. By specifying @true@ for @forceUpdate@, you are
    -- affirming that you have already updated the subscription filters. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Cross-Account-Log_Subscription-Update.html Updating an existing cross-account subscription>
    --
    -- If you omit this parameter, the default of @false@ is used.
    forceUpdate :: Prelude.Maybe Prelude.Bool,
    -- | A name for an existing destination.
    destinationName :: Prelude.Text,
    -- | An IAM policy document that authorizes cross-account users to deliver
    -- their log events to the associated destination. This can be up to 5120
    -- bytes.
    accessPolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDestinationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceUpdate', 'putDestinationPolicy_forceUpdate' - Specify true if you are updating an existing destination policy to grant
-- permission to an organization ID instead of granting permission to
-- individual AWS accounts. Before you update a destination policy this
-- way, you must first update the subscription filters in the accounts that
-- send logs to this destination. If you do not, the subscription filters
-- might stop working. By specifying @true@ for @forceUpdate@, you are
-- affirming that you have already updated the subscription filters. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Cross-Account-Log_Subscription-Update.html Updating an existing cross-account subscription>
--
-- If you omit this parameter, the default of @false@ is used.
--
-- 'destinationName', 'putDestinationPolicy_destinationName' - A name for an existing destination.
--
-- 'accessPolicy', 'putDestinationPolicy_accessPolicy' - An IAM policy document that authorizes cross-account users to deliver
-- their log events to the associated destination. This can be up to 5120
-- bytes.
newPutDestinationPolicy ::
  -- | 'destinationName'
  Prelude.Text ->
  -- | 'accessPolicy'
  Prelude.Text ->
  PutDestinationPolicy
newPutDestinationPolicy
  pDestinationName_
  pAccessPolicy_ =
    PutDestinationPolicy'
      { forceUpdate =
          Prelude.Nothing,
        destinationName = pDestinationName_,
        accessPolicy = pAccessPolicy_
      }

-- | Specify true if you are updating an existing destination policy to grant
-- permission to an organization ID instead of granting permission to
-- individual AWS accounts. Before you update a destination policy this
-- way, you must first update the subscription filters in the accounts that
-- send logs to this destination. If you do not, the subscription filters
-- might stop working. By specifying @true@ for @forceUpdate@, you are
-- affirming that you have already updated the subscription filters. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/Cross-Account-Log_Subscription-Update.html Updating an existing cross-account subscription>
--
-- If you omit this parameter, the default of @false@ is used.
putDestinationPolicy_forceUpdate :: Lens.Lens' PutDestinationPolicy (Prelude.Maybe Prelude.Bool)
putDestinationPolicy_forceUpdate = Lens.lens (\PutDestinationPolicy' {forceUpdate} -> forceUpdate) (\s@PutDestinationPolicy' {} a -> s {forceUpdate = a} :: PutDestinationPolicy)

-- | A name for an existing destination.
putDestinationPolicy_destinationName :: Lens.Lens' PutDestinationPolicy Prelude.Text
putDestinationPolicy_destinationName = Lens.lens (\PutDestinationPolicy' {destinationName} -> destinationName) (\s@PutDestinationPolicy' {} a -> s {destinationName = a} :: PutDestinationPolicy)

-- | An IAM policy document that authorizes cross-account users to deliver
-- their log events to the associated destination. This can be up to 5120
-- bytes.
putDestinationPolicy_accessPolicy :: Lens.Lens' PutDestinationPolicy Prelude.Text
putDestinationPolicy_accessPolicy = Lens.lens (\PutDestinationPolicy' {accessPolicy} -> accessPolicy) (\s@PutDestinationPolicy' {} a -> s {accessPolicy = a} :: PutDestinationPolicy)

instance Core.AWSRequest PutDestinationPolicy where
  type
    AWSResponse PutDestinationPolicy =
      PutDestinationPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutDestinationPolicyResponse'

instance Prelude.Hashable PutDestinationPolicy where
  hashWithSalt _salt PutDestinationPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` forceUpdate
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` accessPolicy

instance Prelude.NFData PutDestinationPolicy where
  rnf PutDestinationPolicy' {..} =
    Prelude.rnf forceUpdate
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf accessPolicy

instance Data.ToHeaders PutDestinationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutDestinationPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDestinationPolicy where
  toJSON PutDestinationPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("forceUpdate" Data..=) Prelude.<$> forceUpdate,
            Prelude.Just
              ("destinationName" Data..= destinationName),
            Prelude.Just ("accessPolicy" Data..= accessPolicy)
          ]
      )

instance Data.ToPath PutDestinationPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutDestinationPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDestinationPolicyResponse' smart constructor.
data PutDestinationPolicyResponse = PutDestinationPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDestinationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutDestinationPolicyResponse ::
  PutDestinationPolicyResponse
newPutDestinationPolicyResponse =
  PutDestinationPolicyResponse'

instance Prelude.NFData PutDestinationPolicyResponse where
  rnf _ = ()
