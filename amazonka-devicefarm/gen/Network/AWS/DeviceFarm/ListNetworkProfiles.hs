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
-- Module      : Network.AWS.DeviceFarm.ListNetworkProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of available network profiles.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListNetworkProfiles
  ( -- * Creating a Request
    ListNetworkProfiles (..),
    newListNetworkProfiles,

    -- * Request Lenses
    listNetworkProfiles_nextToken,
    listNetworkProfiles_type,
    listNetworkProfiles_arn,

    -- * Destructuring the Response
    ListNetworkProfilesResponse (..),
    newListNetworkProfilesResponse,

    -- * Response Lenses
    listNetworkProfilesResponse_nextToken,
    listNetworkProfilesResponse_networkProfiles,
    listNetworkProfilesResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListNetworkProfiles' smart constructor.
data ListNetworkProfiles = ListNetworkProfiles'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of network profile to return information about. Valid values
    -- are listed here.
    type' :: Prelude.Maybe NetworkProfileType,
    -- | The Amazon Resource Name (ARN) of the project for which you want to list
    -- network profiles.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkProfiles_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'type'', 'listNetworkProfiles_type' - The type of network profile to return information about. Valid values
-- are listed here.
--
-- 'arn', 'listNetworkProfiles_arn' - The Amazon Resource Name (ARN) of the project for which you want to list
-- network profiles.
newListNetworkProfiles ::
  -- | 'arn'
  Prelude.Text ->
  ListNetworkProfiles
newListNetworkProfiles pArn_ =
  ListNetworkProfiles'
    { nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listNetworkProfiles_nextToken :: Lens.Lens' ListNetworkProfiles (Prelude.Maybe Prelude.Text)
listNetworkProfiles_nextToken = Lens.lens (\ListNetworkProfiles' {nextToken} -> nextToken) (\s@ListNetworkProfiles' {} a -> s {nextToken = a} :: ListNetworkProfiles)

-- | The type of network profile to return information about. Valid values
-- are listed here.
listNetworkProfiles_type :: Lens.Lens' ListNetworkProfiles (Prelude.Maybe NetworkProfileType)
listNetworkProfiles_type = Lens.lens (\ListNetworkProfiles' {type'} -> type') (\s@ListNetworkProfiles' {} a -> s {type' = a} :: ListNetworkProfiles)

-- | The Amazon Resource Name (ARN) of the project for which you want to list
-- network profiles.
listNetworkProfiles_arn :: Lens.Lens' ListNetworkProfiles Prelude.Text
listNetworkProfiles_arn = Lens.lens (\ListNetworkProfiles' {arn} -> arn) (\s@ListNetworkProfiles' {} a -> s {arn = a} :: ListNetworkProfiles)

instance Pager.AWSPager ListNetworkProfiles where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listNetworkProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listNetworkProfilesResponse_networkProfiles
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listNetworkProfiles_nextToken
          Lens..~ rs
          Lens.^? listNetworkProfilesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListNetworkProfiles where
  type
    Rs ListNetworkProfiles =
      ListNetworkProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworkProfilesResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> ( x Prelude..?> "networkProfiles"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListNetworkProfiles

instance Prelude.NFData ListNetworkProfiles

instance Prelude.ToHeaders ListNetworkProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.ListNetworkProfiles" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListNetworkProfiles where
  toJSON ListNetworkProfiles' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("type" Prelude..=) Prelude.<$> type',
            Prelude.Just ("arn" Prelude..= arn)
          ]
      )

instance Prelude.ToPath ListNetworkProfiles where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListNetworkProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNetworkProfilesResponse' smart constructor.
data ListNetworkProfilesResponse = ListNetworkProfilesResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the available network profiles.
    networkProfiles :: Prelude.Maybe [NetworkProfile],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkProfilesResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'networkProfiles', 'listNetworkProfilesResponse_networkProfiles' - A list of the available network profiles.
--
-- 'httpStatus', 'listNetworkProfilesResponse_httpStatus' - The response's http status code.
newListNetworkProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworkProfilesResponse
newListNetworkProfilesResponse pHttpStatus_ =
  ListNetworkProfilesResponse'
    { nextToken =
        Prelude.Nothing,
      networkProfiles = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listNetworkProfilesResponse_nextToken :: Lens.Lens' ListNetworkProfilesResponse (Prelude.Maybe Prelude.Text)
listNetworkProfilesResponse_nextToken = Lens.lens (\ListNetworkProfilesResponse' {nextToken} -> nextToken) (\s@ListNetworkProfilesResponse' {} a -> s {nextToken = a} :: ListNetworkProfilesResponse)

-- | A list of the available network profiles.
listNetworkProfilesResponse_networkProfiles :: Lens.Lens' ListNetworkProfilesResponse (Prelude.Maybe [NetworkProfile])
listNetworkProfilesResponse_networkProfiles = Lens.lens (\ListNetworkProfilesResponse' {networkProfiles} -> networkProfiles) (\s@ListNetworkProfilesResponse' {} a -> s {networkProfiles = a} :: ListNetworkProfilesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listNetworkProfilesResponse_httpStatus :: Lens.Lens' ListNetworkProfilesResponse Prelude.Int
listNetworkProfilesResponse_httpStatus = Lens.lens (\ListNetworkProfilesResponse' {httpStatus} -> httpStatus) (\s@ListNetworkProfilesResponse' {} a -> s {httpStatus = a} :: ListNetworkProfilesResponse)

instance Prelude.NFData ListNetworkProfilesResponse
