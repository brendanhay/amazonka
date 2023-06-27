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
-- Module      : Amazonka.WellArchitected.ListProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List profiles.
module Amazonka.WellArchitected.ListProfiles
  ( -- * Creating a Request
    ListProfiles (..),
    newListProfiles,

    -- * Request Lenses
    listProfiles_maxResults,
    listProfiles_nextToken,
    listProfiles_profileNamePrefix,
    listProfiles_profileOwnerType,

    -- * Destructuring the Response
    ListProfilesResponse (..),
    newListProfilesResponse,

    -- * Response Lenses
    listProfilesResponse_nextToken,
    listProfilesResponse_profileSummaries,
    listProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newListProfiles' smart constructor.
data ListProfiles = ListProfiles'
  { maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Prefix for profile name.
    profileNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | Profile owner type.
    profileOwnerType :: Prelude.Maybe ProfileOwnerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listProfiles_maxResults' - Undocumented member.
--
-- 'nextToken', 'listProfiles_nextToken' - Undocumented member.
--
-- 'profileNamePrefix', 'listProfiles_profileNamePrefix' - Prefix for profile name.
--
-- 'profileOwnerType', 'listProfiles_profileOwnerType' - Profile owner type.
newListProfiles ::
  ListProfiles
newListProfiles =
  ListProfiles'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      profileNamePrefix = Prelude.Nothing,
      profileOwnerType = Prelude.Nothing
    }

-- | Undocumented member.
listProfiles_maxResults :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Natural)
listProfiles_maxResults = Lens.lens (\ListProfiles' {maxResults} -> maxResults) (\s@ListProfiles' {} a -> s {maxResults = a} :: ListProfiles)

-- | Undocumented member.
listProfiles_nextToken :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Text)
listProfiles_nextToken = Lens.lens (\ListProfiles' {nextToken} -> nextToken) (\s@ListProfiles' {} a -> s {nextToken = a} :: ListProfiles)

-- | Prefix for profile name.
listProfiles_profileNamePrefix :: Lens.Lens' ListProfiles (Prelude.Maybe Prelude.Text)
listProfiles_profileNamePrefix = Lens.lens (\ListProfiles' {profileNamePrefix} -> profileNamePrefix) (\s@ListProfiles' {} a -> s {profileNamePrefix = a} :: ListProfiles)

-- | Profile owner type.
listProfiles_profileOwnerType :: Lens.Lens' ListProfiles (Prelude.Maybe ProfileOwnerType)
listProfiles_profileOwnerType = Lens.lens (\ListProfiles' {profileOwnerType} -> profileOwnerType) (\s@ListProfiles' {} a -> s {profileOwnerType = a} :: ListProfiles)

instance Core.AWSRequest ListProfiles where
  type AWSResponse ListProfiles = ListProfilesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfilesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "ProfileSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListProfiles where
  hashWithSalt _salt ListProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` profileNamePrefix
      `Prelude.hashWithSalt` profileOwnerType

instance Prelude.NFData ListProfiles where
  rnf ListProfiles' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profileNamePrefix
      `Prelude.seq` Prelude.rnf profileOwnerType

instance Data.ToHeaders ListProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListProfiles where
  toPath = Prelude.const "/profileSummaries"

instance Data.ToQuery ListProfiles where
  toQuery ListProfiles' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ProfileNamePrefix" Data.=: profileNamePrefix,
        "ProfileOwnerType" Data.=: profileOwnerType
      ]

-- | /See:/ 'newListProfilesResponse' smart constructor.
data ListProfilesResponse = ListProfilesResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | Profile summaries.
    profileSummaries :: Prelude.Maybe [ProfileSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfilesResponse_nextToken' - Undocumented member.
--
-- 'profileSummaries', 'listProfilesResponse_profileSummaries' - Profile summaries.
--
-- 'httpStatus', 'listProfilesResponse_httpStatus' - The response's http status code.
newListProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfilesResponse
newListProfilesResponse pHttpStatus_ =
  ListProfilesResponse'
    { nextToken = Prelude.Nothing,
      profileSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listProfilesResponse_nextToken :: Lens.Lens' ListProfilesResponse (Prelude.Maybe Prelude.Text)
listProfilesResponse_nextToken = Lens.lens (\ListProfilesResponse' {nextToken} -> nextToken) (\s@ListProfilesResponse' {} a -> s {nextToken = a} :: ListProfilesResponse)

-- | Profile summaries.
listProfilesResponse_profileSummaries :: Lens.Lens' ListProfilesResponse (Prelude.Maybe [ProfileSummary])
listProfilesResponse_profileSummaries = Lens.lens (\ListProfilesResponse' {profileSummaries} -> profileSummaries) (\s@ListProfilesResponse' {} a -> s {profileSummaries = a} :: ListProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listProfilesResponse_httpStatus :: Lens.Lens' ListProfilesResponse Prelude.Int
listProfilesResponse_httpStatus = Lens.lens (\ListProfilesResponse' {httpStatus} -> httpStatus) (\s@ListProfilesResponse' {} a -> s {httpStatus = a} :: ListProfilesResponse)

instance Prelude.NFData ListProfilesResponse where
  rnf ListProfilesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf profileSummaries
      `Prelude.seq` Prelude.rnf httpStatus
