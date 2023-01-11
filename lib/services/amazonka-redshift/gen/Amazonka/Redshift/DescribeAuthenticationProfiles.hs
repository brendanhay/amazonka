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
-- Module      : Amazonka.Redshift.DescribeAuthenticationProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an authentication profile.
module Amazonka.Redshift.DescribeAuthenticationProfiles
  ( -- * Creating a Request
    DescribeAuthenticationProfiles (..),
    newDescribeAuthenticationProfiles,

    -- * Request Lenses
    describeAuthenticationProfiles_authenticationProfileName,

    -- * Destructuring the Response
    DescribeAuthenticationProfilesResponse (..),
    newDescribeAuthenticationProfilesResponse,

    -- * Response Lenses
    describeAuthenticationProfilesResponse_authenticationProfiles,
    describeAuthenticationProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAuthenticationProfiles' smart constructor.
data DescribeAuthenticationProfiles = DescribeAuthenticationProfiles'
  { -- | The name of the authentication profile to describe. If not specified
    -- then all authentication profiles owned by the account are listed.
    authenticationProfileName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuthenticationProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfileName', 'describeAuthenticationProfiles_authenticationProfileName' - The name of the authentication profile to describe. If not specified
-- then all authentication profiles owned by the account are listed.
newDescribeAuthenticationProfiles ::
  DescribeAuthenticationProfiles
newDescribeAuthenticationProfiles =
  DescribeAuthenticationProfiles'
    { authenticationProfileName =
        Prelude.Nothing
    }

-- | The name of the authentication profile to describe. If not specified
-- then all authentication profiles owned by the account are listed.
describeAuthenticationProfiles_authenticationProfileName :: Lens.Lens' DescribeAuthenticationProfiles (Prelude.Maybe Prelude.Text)
describeAuthenticationProfiles_authenticationProfileName = Lens.lens (\DescribeAuthenticationProfiles' {authenticationProfileName} -> authenticationProfileName) (\s@DescribeAuthenticationProfiles' {} a -> s {authenticationProfileName = a} :: DescribeAuthenticationProfiles)

instance
  Core.AWSRequest
    DescribeAuthenticationProfiles
  where
  type
    AWSResponse DescribeAuthenticationProfiles =
      DescribeAuthenticationProfilesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeAuthenticationProfilesResult"
      ( \s h x ->
          DescribeAuthenticationProfilesResponse'
            Prelude.<$> ( x Data..@? "AuthenticationProfiles"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAuthenticationProfiles
  where
  hashWithSalt
    _salt
    DescribeAuthenticationProfiles' {..} =
      _salt
        `Prelude.hashWithSalt` authenticationProfileName

instance
  Prelude.NFData
    DescribeAuthenticationProfiles
  where
  rnf DescribeAuthenticationProfiles' {..} =
    Prelude.rnf authenticationProfileName

instance
  Data.ToHeaders
    DescribeAuthenticationProfiles
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAuthenticationProfiles where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAuthenticationProfiles where
  toQuery DescribeAuthenticationProfiles' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeAuthenticationProfiles" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AuthenticationProfileName"
          Data.=: authenticationProfileName
      ]

-- | /See:/ 'newDescribeAuthenticationProfilesResponse' smart constructor.
data DescribeAuthenticationProfilesResponse = DescribeAuthenticationProfilesResponse'
  { -- | The list of authentication profiles.
    authenticationProfiles :: Prelude.Maybe [AuthenticationProfile],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAuthenticationProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationProfiles', 'describeAuthenticationProfilesResponse_authenticationProfiles' - The list of authentication profiles.
--
-- 'httpStatus', 'describeAuthenticationProfilesResponse_httpStatus' - The response's http status code.
newDescribeAuthenticationProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAuthenticationProfilesResponse
newDescribeAuthenticationProfilesResponse
  pHttpStatus_ =
    DescribeAuthenticationProfilesResponse'
      { authenticationProfiles =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of authentication profiles.
describeAuthenticationProfilesResponse_authenticationProfiles :: Lens.Lens' DescribeAuthenticationProfilesResponse (Prelude.Maybe [AuthenticationProfile])
describeAuthenticationProfilesResponse_authenticationProfiles = Lens.lens (\DescribeAuthenticationProfilesResponse' {authenticationProfiles} -> authenticationProfiles) (\s@DescribeAuthenticationProfilesResponse' {} a -> s {authenticationProfiles = a} :: DescribeAuthenticationProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAuthenticationProfilesResponse_httpStatus :: Lens.Lens' DescribeAuthenticationProfilesResponse Prelude.Int
describeAuthenticationProfilesResponse_httpStatus = Lens.lens (\DescribeAuthenticationProfilesResponse' {httpStatus} -> httpStatus) (\s@DescribeAuthenticationProfilesResponse' {} a -> s {httpStatus = a} :: DescribeAuthenticationProfilesResponse)

instance
  Prelude.NFData
    DescribeAuthenticationProfilesResponse
  where
  rnf DescribeAuthenticationProfilesResponse' {..} =
    Prelude.rnf authenticationProfiles
      `Prelude.seq` Prelude.rnf httpStatus
