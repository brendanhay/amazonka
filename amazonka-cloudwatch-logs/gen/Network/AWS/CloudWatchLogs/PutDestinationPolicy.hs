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
-- Module      : Network.AWS.CloudWatchLogs.PutDestinationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.CloudWatchLogs.PutDestinationPolicy
  ( -- * Creating a Request
    PutDestinationPolicy (..),
    newPutDestinationPolicy,

    -- * Request Lenses
    putDestinationPolicy_destinationName,
    putDestinationPolicy_accessPolicy,

    -- * Destructuring the Response
    PutDestinationPolicyResponse (..),
    newPutDestinationPolicyResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutDestinationPolicy' smart constructor.
data PutDestinationPolicy = PutDestinationPolicy'
  { -- | A name for an existing destination.
    destinationName :: Core.Text,
    -- | An IAM policy document that authorizes cross-account users to deliver
    -- their log events to the associated destination. This can be up to 5120
    -- bytes.
    accessPolicy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutDestinationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationName', 'putDestinationPolicy_destinationName' - A name for an existing destination.
--
-- 'accessPolicy', 'putDestinationPolicy_accessPolicy' - An IAM policy document that authorizes cross-account users to deliver
-- their log events to the associated destination. This can be up to 5120
-- bytes.
newPutDestinationPolicy ::
  -- | 'destinationName'
  Core.Text ->
  -- | 'accessPolicy'
  Core.Text ->
  PutDestinationPolicy
newPutDestinationPolicy
  pDestinationName_
  pAccessPolicy_ =
    PutDestinationPolicy'
      { destinationName =
          pDestinationName_,
        accessPolicy = pAccessPolicy_
      }

-- | A name for an existing destination.
putDestinationPolicy_destinationName :: Lens.Lens' PutDestinationPolicy Core.Text
putDestinationPolicy_destinationName = Lens.lens (\PutDestinationPolicy' {destinationName} -> destinationName) (\s@PutDestinationPolicy' {} a -> s {destinationName = a} :: PutDestinationPolicy)

-- | An IAM policy document that authorizes cross-account users to deliver
-- their log events to the associated destination. This can be up to 5120
-- bytes.
putDestinationPolicy_accessPolicy :: Lens.Lens' PutDestinationPolicy Core.Text
putDestinationPolicy_accessPolicy = Lens.lens (\PutDestinationPolicy' {accessPolicy} -> accessPolicy) (\s@PutDestinationPolicy' {} a -> s {accessPolicy = a} :: PutDestinationPolicy)

instance Core.AWSRequest PutDestinationPolicy where
  type
    AWSResponse PutDestinationPolicy =
      PutDestinationPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutDestinationPolicyResponse'

instance Core.Hashable PutDestinationPolicy

instance Core.NFData PutDestinationPolicy

instance Core.ToHeaders PutDestinationPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.PutDestinationPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutDestinationPolicy where
  toJSON PutDestinationPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("destinationName" Core..= destinationName),
            Core.Just ("accessPolicy" Core..= accessPolicy)
          ]
      )

instance Core.ToPath PutDestinationPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutDestinationPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutDestinationPolicyResponse' smart constructor.
data PutDestinationPolicyResponse = PutDestinationPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutDestinationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutDestinationPolicyResponse ::
  PutDestinationPolicyResponse
newPutDestinationPolicyResponse =
  PutDestinationPolicyResponse'

instance Core.NFData PutDestinationPolicyResponse
