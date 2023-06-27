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
-- Module      : Amazonka.CustomerProfiles.MergeProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an AWS Lambda job that does the following:
--
-- 1.  All the profileKeys in the @ProfileToBeMerged@ will be moved to the
--     main profile.
--
-- 2.  All the objects in the @ProfileToBeMerged@ will be moved to the main
--     profile.
--
-- 3.  All the @ProfileToBeMerged@ will be deleted at the end.
--
-- 4.  All the profileKeys in the @ProfileIdsToBeMerged@ will be moved to
--     the main profile.
--
-- 5.  Standard fields are merged as follows:
--
--     1.  Fields are always \"union\"-ed if there are no conflicts in
--         standard fields or attributeKeys.
--
--     2.  When there are conflicting fields:
--
--         1.  If no @SourceProfileIds@ entry is specified, the main
--             Profile value is always taken.
--
--         2.  If a @SourceProfileIds@ entry is specified, the specified
--             profileId is always taken, even if it is a NULL value.
--
-- You can use MergeProfiles together with
-- <https://docs.aws.amazon.com/customerprofiles/latest/APIReference/API_GetMatches.html GetMatches>,
-- which returns potentially matching profiles, or use it with the results
-- of another matching system. After profiles have been merged, they cannot
-- be separated (unmerged).
module Amazonka.CustomerProfiles.MergeProfiles
  ( -- * Creating a Request
    MergeProfiles (..),
    newMergeProfiles,

    -- * Request Lenses
    mergeProfiles_fieldSourceProfileIds,
    mergeProfiles_domainName,
    mergeProfiles_mainProfileId,
    mergeProfiles_profileIdsToBeMerged,

    -- * Destructuring the Response
    MergeProfilesResponse (..),
    newMergeProfilesResponse,

    -- * Response Lenses
    mergeProfilesResponse_message,
    mergeProfilesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newMergeProfiles' smart constructor.
data MergeProfiles = MergeProfiles'
  { -- | The identifiers of the fields in the profile that has the information
    -- you want to apply to the merge. For example, say you want to merge
    -- EmailAddress from Profile1 into MainProfile. This would be the
    -- identifier of the EmailAddress field in Profile1.
    fieldSourceProfileIds :: Prelude.Maybe FieldSourceProfileIds,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The identifier of the profile to be taken.
    mainProfileId :: Prelude.Text,
    -- | The identifier of the profile to be merged into MainProfileId.
    profileIdsToBeMerged :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldSourceProfileIds', 'mergeProfiles_fieldSourceProfileIds' - The identifiers of the fields in the profile that has the information
-- you want to apply to the merge. For example, say you want to merge
-- EmailAddress from Profile1 into MainProfile. This would be the
-- identifier of the EmailAddress field in Profile1.
--
-- 'domainName', 'mergeProfiles_domainName' - The unique name of the domain.
--
-- 'mainProfileId', 'mergeProfiles_mainProfileId' - The identifier of the profile to be taken.
--
-- 'profileIdsToBeMerged', 'mergeProfiles_profileIdsToBeMerged' - The identifier of the profile to be merged into MainProfileId.
newMergeProfiles ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'mainProfileId'
  Prelude.Text ->
  -- | 'profileIdsToBeMerged'
  Prelude.NonEmpty Prelude.Text ->
  MergeProfiles
newMergeProfiles
  pDomainName_
  pMainProfileId_
  pProfileIdsToBeMerged_ =
    MergeProfiles'
      { fieldSourceProfileIds =
          Prelude.Nothing,
        domainName = pDomainName_,
        mainProfileId = pMainProfileId_,
        profileIdsToBeMerged =
          Lens.coerced Lens.# pProfileIdsToBeMerged_
      }

-- | The identifiers of the fields in the profile that has the information
-- you want to apply to the merge. For example, say you want to merge
-- EmailAddress from Profile1 into MainProfile. This would be the
-- identifier of the EmailAddress field in Profile1.
mergeProfiles_fieldSourceProfileIds :: Lens.Lens' MergeProfiles (Prelude.Maybe FieldSourceProfileIds)
mergeProfiles_fieldSourceProfileIds = Lens.lens (\MergeProfiles' {fieldSourceProfileIds} -> fieldSourceProfileIds) (\s@MergeProfiles' {} a -> s {fieldSourceProfileIds = a} :: MergeProfiles)

-- | The unique name of the domain.
mergeProfiles_domainName :: Lens.Lens' MergeProfiles Prelude.Text
mergeProfiles_domainName = Lens.lens (\MergeProfiles' {domainName} -> domainName) (\s@MergeProfiles' {} a -> s {domainName = a} :: MergeProfiles)

-- | The identifier of the profile to be taken.
mergeProfiles_mainProfileId :: Lens.Lens' MergeProfiles Prelude.Text
mergeProfiles_mainProfileId = Lens.lens (\MergeProfiles' {mainProfileId} -> mainProfileId) (\s@MergeProfiles' {} a -> s {mainProfileId = a} :: MergeProfiles)

-- | The identifier of the profile to be merged into MainProfileId.
mergeProfiles_profileIdsToBeMerged :: Lens.Lens' MergeProfiles (Prelude.NonEmpty Prelude.Text)
mergeProfiles_profileIdsToBeMerged = Lens.lens (\MergeProfiles' {profileIdsToBeMerged} -> profileIdsToBeMerged) (\s@MergeProfiles' {} a -> s {profileIdsToBeMerged = a} :: MergeProfiles) Prelude.. Lens.coerced

instance Core.AWSRequest MergeProfiles where
  type
    AWSResponse MergeProfiles =
      MergeProfilesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          MergeProfilesResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable MergeProfiles where
  hashWithSalt _salt MergeProfiles' {..} =
    _salt
      `Prelude.hashWithSalt` fieldSourceProfileIds
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` mainProfileId
      `Prelude.hashWithSalt` profileIdsToBeMerged

instance Prelude.NFData MergeProfiles where
  rnf MergeProfiles' {..} =
    Prelude.rnf fieldSourceProfileIds
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf mainProfileId
      `Prelude.seq` Prelude.rnf profileIdsToBeMerged

instance Data.ToHeaders MergeProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MergeProfiles where
  toJSON MergeProfiles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldSourceProfileIds" Data..=)
              Prelude.<$> fieldSourceProfileIds,
            Prelude.Just ("MainProfileId" Data..= mainProfileId),
            Prelude.Just
              ( "ProfileIdsToBeMerged"
                  Data..= profileIdsToBeMerged
              )
          ]
      )

instance Data.ToPath MergeProfiles where
  toPath MergeProfiles' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/objects/merge"
      ]

instance Data.ToQuery MergeProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newMergeProfilesResponse' smart constructor.
data MergeProfilesResponse = MergeProfilesResponse'
  { -- | A message that indicates the merge request is complete.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MergeProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'mergeProfilesResponse_message' - A message that indicates the merge request is complete.
--
-- 'httpStatus', 'mergeProfilesResponse_httpStatus' - The response's http status code.
newMergeProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  MergeProfilesResponse
newMergeProfilesResponse pHttpStatus_ =
  MergeProfilesResponse'
    { message = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message that indicates the merge request is complete.
mergeProfilesResponse_message :: Lens.Lens' MergeProfilesResponse (Prelude.Maybe Prelude.Text)
mergeProfilesResponse_message = Lens.lens (\MergeProfilesResponse' {message} -> message) (\s@MergeProfilesResponse' {} a -> s {message = a} :: MergeProfilesResponse)

-- | The response's http status code.
mergeProfilesResponse_httpStatus :: Lens.Lens' MergeProfilesResponse Prelude.Int
mergeProfilesResponse_httpStatus = Lens.lens (\MergeProfilesResponse' {httpStatus} -> httpStatus) (\s@MergeProfilesResponse' {} a -> s {httpStatus = a} :: MergeProfilesResponse)

instance Prelude.NFData MergeProfilesResponse where
  rnf MergeProfilesResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf httpStatus
