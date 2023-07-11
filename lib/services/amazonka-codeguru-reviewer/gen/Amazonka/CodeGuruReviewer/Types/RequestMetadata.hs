{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeGuruReviewer.Types.RequestMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RequestMetadata where

import Amazonka.CodeGuruReviewer.Types.EventInfo
import Amazonka.CodeGuruReviewer.Types.VendorName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Metadata that is associated with a code review. This applies to both
-- pull request and repository analysis code reviews.
--
-- /See:/ 'newRequestMetadata' smart constructor.
data RequestMetadata = RequestMetadata'
  { -- | Information about the event associated with a code review.
    eventInfo :: Prelude.Maybe EventInfo,
    -- | The ID of the request. This is required for a pull request code review.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | An identifier, such as a name or account ID, that is associated with the
    -- requester. The @Requester@ is used to capture the @author\/actor@ name
    -- of the event request.
    requester :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository vendor used to upload code to an S3 bucket
    -- for a CI\/CD code review. For example, if code and artifacts are
    -- uploaded to an S3 bucket for a CI\/CD code review by GitHub scripts from
    -- a GitHub repository, then the repository association\'s @ProviderType@
    -- is @S3Bucket@ and the CI\/CD repository vendor name is GitHub. For more
    -- information, see the definition for @ProviderType@ in
    -- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>.
    vendorName :: Prelude.Maybe VendorName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventInfo', 'requestMetadata_eventInfo' - Information about the event associated with a code review.
--
-- 'requestId', 'requestMetadata_requestId' - The ID of the request. This is required for a pull request code review.
--
-- 'requester', 'requestMetadata_requester' - An identifier, such as a name or account ID, that is associated with the
-- requester. The @Requester@ is used to capture the @author\/actor@ name
-- of the event request.
--
-- 'vendorName', 'requestMetadata_vendorName' - The name of the repository vendor used to upload code to an S3 bucket
-- for a CI\/CD code review. For example, if code and artifacts are
-- uploaded to an S3 bucket for a CI\/CD code review by GitHub scripts from
-- a GitHub repository, then the repository association\'s @ProviderType@
-- is @S3Bucket@ and the CI\/CD repository vendor name is GitHub. For more
-- information, see the definition for @ProviderType@ in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>.
newRequestMetadata ::
  RequestMetadata
newRequestMetadata =
  RequestMetadata'
    { eventInfo = Prelude.Nothing,
      requestId = Prelude.Nothing,
      requester = Prelude.Nothing,
      vendorName = Prelude.Nothing
    }

-- | Information about the event associated with a code review.
requestMetadata_eventInfo :: Lens.Lens' RequestMetadata (Prelude.Maybe EventInfo)
requestMetadata_eventInfo = Lens.lens (\RequestMetadata' {eventInfo} -> eventInfo) (\s@RequestMetadata' {} a -> s {eventInfo = a} :: RequestMetadata)

-- | The ID of the request. This is required for a pull request code review.
requestMetadata_requestId :: Lens.Lens' RequestMetadata (Prelude.Maybe Prelude.Text)
requestMetadata_requestId = Lens.lens (\RequestMetadata' {requestId} -> requestId) (\s@RequestMetadata' {} a -> s {requestId = a} :: RequestMetadata)

-- | An identifier, such as a name or account ID, that is associated with the
-- requester. The @Requester@ is used to capture the @author\/actor@ name
-- of the event request.
requestMetadata_requester :: Lens.Lens' RequestMetadata (Prelude.Maybe Prelude.Text)
requestMetadata_requester = Lens.lens (\RequestMetadata' {requester} -> requester) (\s@RequestMetadata' {} a -> s {requester = a} :: RequestMetadata)

-- | The name of the repository vendor used to upload code to an S3 bucket
-- for a CI\/CD code review. For example, if code and artifacts are
-- uploaded to an S3 bucket for a CI\/CD code review by GitHub scripts from
-- a GitHub repository, then the repository association\'s @ProviderType@
-- is @S3Bucket@ and the CI\/CD repository vendor name is GitHub. For more
-- information, see the definition for @ProviderType@ in
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_RepositoryAssociation.html RepositoryAssociation>.
requestMetadata_vendorName :: Lens.Lens' RequestMetadata (Prelude.Maybe VendorName)
requestMetadata_vendorName = Lens.lens (\RequestMetadata' {vendorName} -> vendorName) (\s@RequestMetadata' {} a -> s {vendorName = a} :: RequestMetadata)

instance Data.FromJSON RequestMetadata where
  parseJSON =
    Data.withObject
      "RequestMetadata"
      ( \x ->
          RequestMetadata'
            Prelude.<$> (x Data..:? "EventInfo")
            Prelude.<*> (x Data..:? "RequestId")
            Prelude.<*> (x Data..:? "Requester")
            Prelude.<*> (x Data..:? "VendorName")
      )

instance Prelude.Hashable RequestMetadata where
  hashWithSalt _salt RequestMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` eventInfo
      `Prelude.hashWithSalt` requestId
      `Prelude.hashWithSalt` requester
      `Prelude.hashWithSalt` vendorName

instance Prelude.NFData RequestMetadata where
  rnf RequestMetadata' {..} =
    Prelude.rnf eventInfo
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf requester
      `Prelude.seq` Prelude.rnf vendorName

instance Data.ToJSON RequestMetadata where
  toJSON RequestMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EventInfo" Data..=) Prelude.<$> eventInfo,
            ("RequestId" Data..=) Prelude.<$> requestId,
            ("Requester" Data..=) Prelude.<$> requester,
            ("VendorName" Data..=) Prelude.<$> vendorName
          ]
      )
