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
-- Module      : Amazonka.ChimeSDKMessaging.BatchCreateChannelMembership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a specified number of users to a channel.
module Amazonka.ChimeSDKMessaging.BatchCreateChannelMembership
  ( -- * Creating a Request
    BatchCreateChannelMembership (..),
    newBatchCreateChannelMembership,

    -- * Request Lenses
    batchCreateChannelMembership_subChannelId,
    batchCreateChannelMembership_type,
    batchCreateChannelMembership_channelArn,
    batchCreateChannelMembership_memberArns,
    batchCreateChannelMembership_chimeBearer,

    -- * Destructuring the Response
    BatchCreateChannelMembershipResponse (..),
    newBatchCreateChannelMembershipResponse,

    -- * Response Lenses
    batchCreateChannelMembershipResponse_batchChannelMemberships,
    batchCreateChannelMembershipResponse_errors,
    batchCreateChannelMembershipResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreateChannelMembership' smart constructor.
data BatchCreateChannelMembership = BatchCreateChannelMembership'
  { -- | The ID of the SubChannel in the request.
    --
    -- Only required when creating membership in a SubChannel for a moderator
    -- in an elastic channel.
    subChannelId :: Prelude.Maybe Prelude.Text,
    -- | The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
    -- are always returned as part of @ListChannelMemberships@. Hidden members
    -- are only returned if the type filter in @ListChannelMemberships@ equals
    -- @HIDDEN@. Otherwise hidden members are not returned. This is only
    -- supported by moderators.
    type' :: Prelude.Maybe ChannelMembershipType,
    -- | The ARN of the channel to which you\'re adding users.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@s of the members you want to add to the channel.
    memberArns :: Prelude.NonEmpty Prelude.Text,
    -- | The @AppInstanceUserArn@ of the user that makes the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateChannelMembership' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subChannelId', 'batchCreateChannelMembership_subChannelId' - The ID of the SubChannel in the request.
--
-- Only required when creating membership in a SubChannel for a moderator
-- in an elastic channel.
--
-- 'type'', 'batchCreateChannelMembership_type' - The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
-- are always returned as part of @ListChannelMemberships@. Hidden members
-- are only returned if the type filter in @ListChannelMemberships@ equals
-- @HIDDEN@. Otherwise hidden members are not returned. This is only
-- supported by moderators.
--
-- 'channelArn', 'batchCreateChannelMembership_channelArn' - The ARN of the channel to which you\'re adding users.
--
-- 'memberArns', 'batchCreateChannelMembership_memberArns' - The @AppInstanceUserArn@s of the members you want to add to the channel.
--
-- 'chimeBearer', 'batchCreateChannelMembership_chimeBearer' - The @AppInstanceUserArn@ of the user that makes the API call.
newBatchCreateChannelMembership ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  BatchCreateChannelMembership
newBatchCreateChannelMembership
  pChannelArn_
  pMemberArns_
  pChimeBearer_ =
    BatchCreateChannelMembership'
      { subChannelId =
          Prelude.Nothing,
        type' = Prelude.Nothing,
        channelArn = pChannelArn_,
        memberArns = Lens.coerced Lens.# pMemberArns_,
        chimeBearer = pChimeBearer_
      }

-- | The ID of the SubChannel in the request.
--
-- Only required when creating membership in a SubChannel for a moderator
-- in an elastic channel.
batchCreateChannelMembership_subChannelId :: Lens.Lens' BatchCreateChannelMembership (Prelude.Maybe Prelude.Text)
batchCreateChannelMembership_subChannelId = Lens.lens (\BatchCreateChannelMembership' {subChannelId} -> subChannelId) (\s@BatchCreateChannelMembership' {} a -> s {subChannelId = a} :: BatchCreateChannelMembership)

-- | The membership type of a user, @DEFAULT@ or @HIDDEN@. Default members
-- are always returned as part of @ListChannelMemberships@. Hidden members
-- are only returned if the type filter in @ListChannelMemberships@ equals
-- @HIDDEN@. Otherwise hidden members are not returned. This is only
-- supported by moderators.
batchCreateChannelMembership_type :: Lens.Lens' BatchCreateChannelMembership (Prelude.Maybe ChannelMembershipType)
batchCreateChannelMembership_type = Lens.lens (\BatchCreateChannelMembership' {type'} -> type') (\s@BatchCreateChannelMembership' {} a -> s {type' = a} :: BatchCreateChannelMembership)

-- | The ARN of the channel to which you\'re adding users.
batchCreateChannelMembership_channelArn :: Lens.Lens' BatchCreateChannelMembership Prelude.Text
batchCreateChannelMembership_channelArn = Lens.lens (\BatchCreateChannelMembership' {channelArn} -> channelArn) (\s@BatchCreateChannelMembership' {} a -> s {channelArn = a} :: BatchCreateChannelMembership)

-- | The @AppInstanceUserArn@s of the members you want to add to the channel.
batchCreateChannelMembership_memberArns :: Lens.Lens' BatchCreateChannelMembership (Prelude.NonEmpty Prelude.Text)
batchCreateChannelMembership_memberArns = Lens.lens (\BatchCreateChannelMembership' {memberArns} -> memberArns) (\s@BatchCreateChannelMembership' {} a -> s {memberArns = a} :: BatchCreateChannelMembership) Prelude.. Lens.coerced

-- | The @AppInstanceUserArn@ of the user that makes the API call.
batchCreateChannelMembership_chimeBearer :: Lens.Lens' BatchCreateChannelMembership Prelude.Text
batchCreateChannelMembership_chimeBearer = Lens.lens (\BatchCreateChannelMembership' {chimeBearer} -> chimeBearer) (\s@BatchCreateChannelMembership' {} a -> s {chimeBearer = a} :: BatchCreateChannelMembership)

instance Core.AWSRequest BatchCreateChannelMembership where
  type
    AWSResponse BatchCreateChannelMembership =
      BatchCreateChannelMembershipResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateChannelMembershipResponse'
            Prelude.<$> (x Data..?> "BatchChannelMemberships")
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchCreateChannelMembership
  where
  hashWithSalt _salt BatchCreateChannelMembership' {..} =
    _salt
      `Prelude.hashWithSalt` subChannelId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` channelArn
      `Prelude.hashWithSalt` memberArns
      `Prelude.hashWithSalt` chimeBearer

instance Prelude.NFData BatchCreateChannelMembership where
  rnf BatchCreateChannelMembership' {..} =
    Prelude.rnf subChannelId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf channelArn
      `Prelude.seq` Prelude.rnf memberArns
      `Prelude.seq` Prelude.rnf chimeBearer

instance Data.ToHeaders BatchCreateChannelMembership where
  toHeaders BatchCreateChannelMembership' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON BatchCreateChannelMembership where
  toJSON BatchCreateChannelMembership' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SubChannelId" Data..=) Prelude.<$> subChannelId,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just ("MemberArns" Data..= memberArns)
          ]
      )

instance Data.ToPath BatchCreateChannelMembership where
  toPath BatchCreateChannelMembership' {..} =
    Prelude.mconcat
      ["/channels/", Data.toBS channelArn, "/memberships"]

instance Data.ToQuery BatchCreateChannelMembership where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=batch-create"])

-- | /See:/ 'newBatchCreateChannelMembershipResponse' smart constructor.
data BatchCreateChannelMembershipResponse = BatchCreateChannelMembershipResponse'
  { -- | The list of channel memberships in the response.
    batchChannelMemberships :: Prelude.Maybe BatchChannelMemberships,
    -- | If the action fails for one or more of the memberships in the request, a
    -- list of the memberships is returned, along with error codes and error
    -- messages.
    errors :: Prelude.Maybe [BatchCreateChannelMembershipError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateChannelMembershipResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchChannelMemberships', 'batchCreateChannelMembershipResponse_batchChannelMemberships' - The list of channel memberships in the response.
--
-- 'errors', 'batchCreateChannelMembershipResponse_errors' - If the action fails for one or more of the memberships in the request, a
-- list of the memberships is returned, along with error codes and error
-- messages.
--
-- 'httpStatus', 'batchCreateChannelMembershipResponse_httpStatus' - The response's http status code.
newBatchCreateChannelMembershipResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchCreateChannelMembershipResponse
newBatchCreateChannelMembershipResponse pHttpStatus_ =
  BatchCreateChannelMembershipResponse'
    { batchChannelMemberships =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of channel memberships in the response.
batchCreateChannelMembershipResponse_batchChannelMemberships :: Lens.Lens' BatchCreateChannelMembershipResponse (Prelude.Maybe BatchChannelMemberships)
batchCreateChannelMembershipResponse_batchChannelMemberships = Lens.lens (\BatchCreateChannelMembershipResponse' {batchChannelMemberships} -> batchChannelMemberships) (\s@BatchCreateChannelMembershipResponse' {} a -> s {batchChannelMemberships = a} :: BatchCreateChannelMembershipResponse)

-- | If the action fails for one or more of the memberships in the request, a
-- list of the memberships is returned, along with error codes and error
-- messages.
batchCreateChannelMembershipResponse_errors :: Lens.Lens' BatchCreateChannelMembershipResponse (Prelude.Maybe [BatchCreateChannelMembershipError])
batchCreateChannelMembershipResponse_errors = Lens.lens (\BatchCreateChannelMembershipResponse' {errors} -> errors) (\s@BatchCreateChannelMembershipResponse' {} a -> s {errors = a} :: BatchCreateChannelMembershipResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateChannelMembershipResponse_httpStatus :: Lens.Lens' BatchCreateChannelMembershipResponse Prelude.Int
batchCreateChannelMembershipResponse_httpStatus = Lens.lens (\BatchCreateChannelMembershipResponse' {httpStatus} -> httpStatus) (\s@BatchCreateChannelMembershipResponse' {} a -> s {httpStatus = a} :: BatchCreateChannelMembershipResponse)

instance
  Prelude.NFData
    BatchCreateChannelMembershipResponse
  where
  rnf BatchCreateChannelMembershipResponse' {..} =
    Prelude.rnf batchChannelMemberships
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
