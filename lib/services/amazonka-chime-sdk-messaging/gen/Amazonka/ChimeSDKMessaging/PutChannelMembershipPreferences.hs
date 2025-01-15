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
-- Module      : Amazonka.ChimeSDKMessaging.PutChannelMembershipPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the membership preferences of an @AppInstanceUser@ for the
-- specified channel. The @AppInstanceUser@ must be a member of the
-- channel. Only the @AppInstanceUser@ who owns the membership can set
-- preferences. Users in the @AppInstanceAdmin@ and channel moderator roles
-- can\'t set preferences for other users. Banned users can\'t set
-- membership preferences for the channel from which they are banned.
module Amazonka.ChimeSDKMessaging.PutChannelMembershipPreferences
  ( -- * Creating a Request
    PutChannelMembershipPreferences (..),
    newPutChannelMembershipPreferences,

    -- * Request Lenses
    putChannelMembershipPreferences_channelArn,
    putChannelMembershipPreferences_memberArn,
    putChannelMembershipPreferences_chimeBearer,
    putChannelMembershipPreferences_preferences,

    -- * Destructuring the Response
    PutChannelMembershipPreferencesResponse (..),
    newPutChannelMembershipPreferencesResponse,

    -- * Response Lenses
    putChannelMembershipPreferencesResponse_channelArn,
    putChannelMembershipPreferencesResponse_member,
    putChannelMembershipPreferencesResponse_preferences,
    putChannelMembershipPreferencesResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutChannelMembershipPreferences' smart constructor.
data PutChannelMembershipPreferences = PutChannelMembershipPreferences'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the member setting the preferences.
    memberArn :: Prelude.Text,
    -- | The @AppInstanceUserARN@ of the user making the API call.
    chimeBearer :: Prelude.Text,
    -- | The channel membership preferences of an @AppInstanceUser@ .
    preferences :: ChannelMembershipPreferences
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChannelMembershipPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'putChannelMembershipPreferences_channelArn' - The ARN of the channel.
--
-- 'memberArn', 'putChannelMembershipPreferences_memberArn' - The @AppInstanceUserArn@ of the member setting the preferences.
--
-- 'chimeBearer', 'putChannelMembershipPreferences_chimeBearer' - The @AppInstanceUserARN@ of the user making the API call.
--
-- 'preferences', 'putChannelMembershipPreferences_preferences' - The channel membership preferences of an @AppInstanceUser@ .
newPutChannelMembershipPreferences ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  -- | 'preferences'
  ChannelMembershipPreferences ->
  PutChannelMembershipPreferences
newPutChannelMembershipPreferences
  pChannelArn_
  pMemberArn_
  pChimeBearer_
  pPreferences_ =
    PutChannelMembershipPreferences'
      { channelArn =
          pChannelArn_,
        memberArn = pMemberArn_,
        chimeBearer = pChimeBearer_,
        preferences = pPreferences_
      }

-- | The ARN of the channel.
putChannelMembershipPreferences_channelArn :: Lens.Lens' PutChannelMembershipPreferences Prelude.Text
putChannelMembershipPreferences_channelArn = Lens.lens (\PutChannelMembershipPreferences' {channelArn} -> channelArn) (\s@PutChannelMembershipPreferences' {} a -> s {channelArn = a} :: PutChannelMembershipPreferences)

-- | The @AppInstanceUserArn@ of the member setting the preferences.
putChannelMembershipPreferences_memberArn :: Lens.Lens' PutChannelMembershipPreferences Prelude.Text
putChannelMembershipPreferences_memberArn = Lens.lens (\PutChannelMembershipPreferences' {memberArn} -> memberArn) (\s@PutChannelMembershipPreferences' {} a -> s {memberArn = a} :: PutChannelMembershipPreferences)

-- | The @AppInstanceUserARN@ of the user making the API call.
putChannelMembershipPreferences_chimeBearer :: Lens.Lens' PutChannelMembershipPreferences Prelude.Text
putChannelMembershipPreferences_chimeBearer = Lens.lens (\PutChannelMembershipPreferences' {chimeBearer} -> chimeBearer) (\s@PutChannelMembershipPreferences' {} a -> s {chimeBearer = a} :: PutChannelMembershipPreferences)

-- | The channel membership preferences of an @AppInstanceUser@ .
putChannelMembershipPreferences_preferences :: Lens.Lens' PutChannelMembershipPreferences ChannelMembershipPreferences
putChannelMembershipPreferences_preferences = Lens.lens (\PutChannelMembershipPreferences' {preferences} -> preferences) (\s@PutChannelMembershipPreferences' {} a -> s {preferences = a} :: PutChannelMembershipPreferences)

instance
  Core.AWSRequest
    PutChannelMembershipPreferences
  where
  type
    AWSResponse PutChannelMembershipPreferences =
      PutChannelMembershipPreferencesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutChannelMembershipPreferencesResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Member")
            Prelude.<*> (x Data..?> "Preferences")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutChannelMembershipPreferences
  where
  hashWithSalt
    _salt
    PutChannelMembershipPreferences' {..} =
      _salt
        `Prelude.hashWithSalt` channelArn
        `Prelude.hashWithSalt` memberArn
        `Prelude.hashWithSalt` chimeBearer
        `Prelude.hashWithSalt` preferences

instance
  Prelude.NFData
    PutChannelMembershipPreferences
  where
  rnf PutChannelMembershipPreferences' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf memberArn `Prelude.seq`
        Prelude.rnf chimeBearer `Prelude.seq`
          Prelude.rnf preferences

instance
  Data.ToHeaders
    PutChannelMembershipPreferences
  where
  toHeaders PutChannelMembershipPreferences' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToJSON PutChannelMembershipPreferences where
  toJSON PutChannelMembershipPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Preferences" Data..= preferences)]
      )

instance Data.ToPath PutChannelMembershipPreferences where
  toPath PutChannelMembershipPreferences' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/memberships/",
        Data.toBS memberArn,
        "/preferences"
      ]

instance Data.ToQuery PutChannelMembershipPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutChannelMembershipPreferencesResponse' smart constructor.
data PutChannelMembershipPreferencesResponse = PutChannelMembershipPreferencesResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The details of a user.
    member :: Prelude.Maybe Identity,
    -- | The ARN and metadata of the member being added.
    preferences :: Prelude.Maybe ChannelMembershipPreferences,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutChannelMembershipPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'putChannelMembershipPreferencesResponse_channelArn' - The ARN of the channel.
--
-- 'member', 'putChannelMembershipPreferencesResponse_member' - The details of a user.
--
-- 'preferences', 'putChannelMembershipPreferencesResponse_preferences' - The ARN and metadata of the member being added.
--
-- 'httpStatus', 'putChannelMembershipPreferencesResponse_httpStatus' - The response's http status code.
newPutChannelMembershipPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutChannelMembershipPreferencesResponse
newPutChannelMembershipPreferencesResponse
  pHttpStatus_ =
    PutChannelMembershipPreferencesResponse'
      { channelArn =
          Prelude.Nothing,
        member = Prelude.Nothing,
        preferences = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the channel.
putChannelMembershipPreferencesResponse_channelArn :: Lens.Lens' PutChannelMembershipPreferencesResponse (Prelude.Maybe Prelude.Text)
putChannelMembershipPreferencesResponse_channelArn = Lens.lens (\PutChannelMembershipPreferencesResponse' {channelArn} -> channelArn) (\s@PutChannelMembershipPreferencesResponse' {} a -> s {channelArn = a} :: PutChannelMembershipPreferencesResponse)

-- | The details of a user.
putChannelMembershipPreferencesResponse_member :: Lens.Lens' PutChannelMembershipPreferencesResponse (Prelude.Maybe Identity)
putChannelMembershipPreferencesResponse_member = Lens.lens (\PutChannelMembershipPreferencesResponse' {member} -> member) (\s@PutChannelMembershipPreferencesResponse' {} a -> s {member = a} :: PutChannelMembershipPreferencesResponse)

-- | The ARN and metadata of the member being added.
putChannelMembershipPreferencesResponse_preferences :: Lens.Lens' PutChannelMembershipPreferencesResponse (Prelude.Maybe ChannelMembershipPreferences)
putChannelMembershipPreferencesResponse_preferences = Lens.lens (\PutChannelMembershipPreferencesResponse' {preferences} -> preferences) (\s@PutChannelMembershipPreferencesResponse' {} a -> s {preferences = a} :: PutChannelMembershipPreferencesResponse)

-- | The response's http status code.
putChannelMembershipPreferencesResponse_httpStatus :: Lens.Lens' PutChannelMembershipPreferencesResponse Prelude.Int
putChannelMembershipPreferencesResponse_httpStatus = Lens.lens (\PutChannelMembershipPreferencesResponse' {httpStatus} -> httpStatus) (\s@PutChannelMembershipPreferencesResponse' {} a -> s {httpStatus = a} :: PutChannelMembershipPreferencesResponse)

instance
  Prelude.NFData
    PutChannelMembershipPreferencesResponse
  where
  rnf PutChannelMembershipPreferencesResponse' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf member `Prelude.seq`
        Prelude.rnf preferences `Prelude.seq`
          Prelude.rnf httpStatus
