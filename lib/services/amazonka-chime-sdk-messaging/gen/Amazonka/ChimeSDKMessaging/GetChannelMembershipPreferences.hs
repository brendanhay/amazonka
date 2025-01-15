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
-- Module      : Amazonka.ChimeSDKMessaging.GetChannelMembershipPreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the membership preferences of an @AppInstanceUser@ for the
-- specified channel. The @AppInstanceUser@ must be a member of the
-- channel. Only the @AppInstanceUser@ who owns the membership can retrieve
-- preferences. Users in the @AppInstanceAdmin@ and channel moderator roles
-- can\'t retrieve preferences for other users. Banned users can\'t
-- retrieve membership preferences for the channel from which they are
-- banned.
module Amazonka.ChimeSDKMessaging.GetChannelMembershipPreferences
  ( -- * Creating a Request
    GetChannelMembershipPreferences (..),
    newGetChannelMembershipPreferences,

    -- * Request Lenses
    getChannelMembershipPreferences_channelArn,
    getChannelMembershipPreferences_memberArn,
    getChannelMembershipPreferences_chimeBearer,

    -- * Destructuring the Response
    GetChannelMembershipPreferencesResponse (..),
    newGetChannelMembershipPreferencesResponse,

    -- * Response Lenses
    getChannelMembershipPreferencesResponse_channelArn,
    getChannelMembershipPreferencesResponse_member,
    getChannelMembershipPreferencesResponse_preferences,
    getChannelMembershipPreferencesResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetChannelMembershipPreferences' smart constructor.
data GetChannelMembershipPreferences = GetChannelMembershipPreferences'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Text,
    -- | The @AppInstanceUserArn@ of the member retrieving the preferences.
    memberArn :: Prelude.Text,
    -- | The @AppInstanceUserARN@ of the user making the API call.
    chimeBearer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelMembershipPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'getChannelMembershipPreferences_channelArn' - The ARN of the channel.
--
-- 'memberArn', 'getChannelMembershipPreferences_memberArn' - The @AppInstanceUserArn@ of the member retrieving the preferences.
--
-- 'chimeBearer', 'getChannelMembershipPreferences_chimeBearer' - The @AppInstanceUserARN@ of the user making the API call.
newGetChannelMembershipPreferences ::
  -- | 'channelArn'
  Prelude.Text ->
  -- | 'memberArn'
  Prelude.Text ->
  -- | 'chimeBearer'
  Prelude.Text ->
  GetChannelMembershipPreferences
newGetChannelMembershipPreferences
  pChannelArn_
  pMemberArn_
  pChimeBearer_ =
    GetChannelMembershipPreferences'
      { channelArn =
          pChannelArn_,
        memberArn = pMemberArn_,
        chimeBearer = pChimeBearer_
      }

-- | The ARN of the channel.
getChannelMembershipPreferences_channelArn :: Lens.Lens' GetChannelMembershipPreferences Prelude.Text
getChannelMembershipPreferences_channelArn = Lens.lens (\GetChannelMembershipPreferences' {channelArn} -> channelArn) (\s@GetChannelMembershipPreferences' {} a -> s {channelArn = a} :: GetChannelMembershipPreferences)

-- | The @AppInstanceUserArn@ of the member retrieving the preferences.
getChannelMembershipPreferences_memberArn :: Lens.Lens' GetChannelMembershipPreferences Prelude.Text
getChannelMembershipPreferences_memberArn = Lens.lens (\GetChannelMembershipPreferences' {memberArn} -> memberArn) (\s@GetChannelMembershipPreferences' {} a -> s {memberArn = a} :: GetChannelMembershipPreferences)

-- | The @AppInstanceUserARN@ of the user making the API call.
getChannelMembershipPreferences_chimeBearer :: Lens.Lens' GetChannelMembershipPreferences Prelude.Text
getChannelMembershipPreferences_chimeBearer = Lens.lens (\GetChannelMembershipPreferences' {chimeBearer} -> chimeBearer) (\s@GetChannelMembershipPreferences' {} a -> s {chimeBearer = a} :: GetChannelMembershipPreferences)

instance
  Core.AWSRequest
    GetChannelMembershipPreferences
  where
  type
    AWSResponse GetChannelMembershipPreferences =
      GetChannelMembershipPreferencesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetChannelMembershipPreferencesResponse'
            Prelude.<$> (x Data..?> "ChannelArn")
            Prelude.<*> (x Data..?> "Member")
            Prelude.<*> (x Data..?> "Preferences")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetChannelMembershipPreferences
  where
  hashWithSalt
    _salt
    GetChannelMembershipPreferences' {..} =
      _salt
        `Prelude.hashWithSalt` channelArn
        `Prelude.hashWithSalt` memberArn
        `Prelude.hashWithSalt` chimeBearer

instance
  Prelude.NFData
    GetChannelMembershipPreferences
  where
  rnf GetChannelMembershipPreferences' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf memberArn `Prelude.seq`
        Prelude.rnf chimeBearer

instance
  Data.ToHeaders
    GetChannelMembershipPreferences
  where
  toHeaders GetChannelMembershipPreferences' {..} =
    Prelude.mconcat
      ["x-amz-chime-bearer" Data.=# chimeBearer]

instance Data.ToPath GetChannelMembershipPreferences where
  toPath GetChannelMembershipPreferences' {..} =
    Prelude.mconcat
      [ "/channels/",
        Data.toBS channelArn,
        "/memberships/",
        Data.toBS memberArn,
        "/preferences"
      ]

instance Data.ToQuery GetChannelMembershipPreferences where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetChannelMembershipPreferencesResponse' smart constructor.
data GetChannelMembershipPreferencesResponse = GetChannelMembershipPreferencesResponse'
  { -- | The ARN of the channel.
    channelArn :: Prelude.Maybe Prelude.Text,
    -- | The details of a user.
    member :: Prelude.Maybe Identity,
    -- | The channel membership preferences for an @AppInstanceUser@ .
    preferences :: Prelude.Maybe ChannelMembershipPreferences,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChannelMembershipPreferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelArn', 'getChannelMembershipPreferencesResponse_channelArn' - The ARN of the channel.
--
-- 'member', 'getChannelMembershipPreferencesResponse_member' - The details of a user.
--
-- 'preferences', 'getChannelMembershipPreferencesResponse_preferences' - The channel membership preferences for an @AppInstanceUser@ .
--
-- 'httpStatus', 'getChannelMembershipPreferencesResponse_httpStatus' - The response's http status code.
newGetChannelMembershipPreferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetChannelMembershipPreferencesResponse
newGetChannelMembershipPreferencesResponse
  pHttpStatus_ =
    GetChannelMembershipPreferencesResponse'
      { channelArn =
          Prelude.Nothing,
        member = Prelude.Nothing,
        preferences = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the channel.
getChannelMembershipPreferencesResponse_channelArn :: Lens.Lens' GetChannelMembershipPreferencesResponse (Prelude.Maybe Prelude.Text)
getChannelMembershipPreferencesResponse_channelArn = Lens.lens (\GetChannelMembershipPreferencesResponse' {channelArn} -> channelArn) (\s@GetChannelMembershipPreferencesResponse' {} a -> s {channelArn = a} :: GetChannelMembershipPreferencesResponse)

-- | The details of a user.
getChannelMembershipPreferencesResponse_member :: Lens.Lens' GetChannelMembershipPreferencesResponse (Prelude.Maybe Identity)
getChannelMembershipPreferencesResponse_member = Lens.lens (\GetChannelMembershipPreferencesResponse' {member} -> member) (\s@GetChannelMembershipPreferencesResponse' {} a -> s {member = a} :: GetChannelMembershipPreferencesResponse)

-- | The channel membership preferences for an @AppInstanceUser@ .
getChannelMembershipPreferencesResponse_preferences :: Lens.Lens' GetChannelMembershipPreferencesResponse (Prelude.Maybe ChannelMembershipPreferences)
getChannelMembershipPreferencesResponse_preferences = Lens.lens (\GetChannelMembershipPreferencesResponse' {preferences} -> preferences) (\s@GetChannelMembershipPreferencesResponse' {} a -> s {preferences = a} :: GetChannelMembershipPreferencesResponse)

-- | The response's http status code.
getChannelMembershipPreferencesResponse_httpStatus :: Lens.Lens' GetChannelMembershipPreferencesResponse Prelude.Int
getChannelMembershipPreferencesResponse_httpStatus = Lens.lens (\GetChannelMembershipPreferencesResponse' {httpStatus} -> httpStatus) (\s@GetChannelMembershipPreferencesResponse' {} a -> s {httpStatus = a} :: GetChannelMembershipPreferencesResponse)

instance
  Prelude.NFData
    GetChannelMembershipPreferencesResponse
  where
  rnf GetChannelMembershipPreferencesResponse' {..} =
    Prelude.rnf channelArn `Prelude.seq`
      Prelude.rnf member `Prelude.seq`
        Prelude.rnf preferences `Prelude.seq`
          Prelude.rnf httpStatus
