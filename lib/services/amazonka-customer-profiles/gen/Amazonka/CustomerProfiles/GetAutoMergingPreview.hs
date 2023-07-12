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
-- Module      : Amazonka.CustomerProfiles.GetAutoMergingPreview
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests the auto-merging settings of your Identity Resolution Job without
-- merging your data. It randomly selects a sample of matching groups from
-- the existing matching results, and applies the automerging settings that
-- you provided. You can then view the number of profiles in the sample,
-- the number of matches, and the number of profiles identified to be
-- merged. This enables you to evaluate the accuracy of the attributes in
-- your matching list.
--
-- You can\'t view which profiles are matched and would be merged.
--
-- We strongly recommend you use this API to do a dry run of the
-- automerging process before running the Identity Resolution Job. Include
-- __at least__ two matching attributes. If your matching list includes too
-- few attributes (such as only @FirstName@ or only @LastName@), there may
-- be a large number of matches. This increases the chances of erroneous
-- merges.
module Amazonka.CustomerProfiles.GetAutoMergingPreview
  ( -- * Creating a Request
    GetAutoMergingPreview (..),
    newGetAutoMergingPreview,

    -- * Request Lenses
    getAutoMergingPreview_minAllowedConfidenceScoreForMerging,
    getAutoMergingPreview_domainName,
    getAutoMergingPreview_consolidation,
    getAutoMergingPreview_conflictResolution,

    -- * Destructuring the Response
    GetAutoMergingPreviewResponse (..),
    newGetAutoMergingPreviewResponse,

    -- * Response Lenses
    getAutoMergingPreviewResponse_numberOfMatchesInSample,
    getAutoMergingPreviewResponse_numberOfProfilesInSample,
    getAutoMergingPreviewResponse_numberOfProfilesWillBeMerged,
    getAutoMergingPreviewResponse_httpStatus,
    getAutoMergingPreviewResponse_domainName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAutoMergingPreview' smart constructor.
data GetAutoMergingPreview = GetAutoMergingPreview'
  { -- | Minimum confidence score required for profiles within a matching group
    -- to be merged during the auto-merge process.
    minAllowedConfidenceScoreForMerging :: Prelude.Maybe Prelude.Double,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | A list of matching attributes that represent matching criteria.
    consolidation :: Consolidation,
    -- | How the auto-merging process should resolve conflicts between different
    -- profiles.
    conflictResolution :: ConflictResolution
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoMergingPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minAllowedConfidenceScoreForMerging', 'getAutoMergingPreview_minAllowedConfidenceScoreForMerging' - Minimum confidence score required for profiles within a matching group
-- to be merged during the auto-merge process.
--
-- 'domainName', 'getAutoMergingPreview_domainName' - The unique name of the domain.
--
-- 'consolidation', 'getAutoMergingPreview_consolidation' - A list of matching attributes that represent matching criteria.
--
-- 'conflictResolution', 'getAutoMergingPreview_conflictResolution' - How the auto-merging process should resolve conflicts between different
-- profiles.
newGetAutoMergingPreview ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'consolidation'
  Consolidation ->
  -- | 'conflictResolution'
  ConflictResolution ->
  GetAutoMergingPreview
newGetAutoMergingPreview
  pDomainName_
  pConsolidation_
  pConflictResolution_ =
    GetAutoMergingPreview'
      { minAllowedConfidenceScoreForMerging =
          Prelude.Nothing,
        domainName = pDomainName_,
        consolidation = pConsolidation_,
        conflictResolution = pConflictResolution_
      }

-- | Minimum confidence score required for profiles within a matching group
-- to be merged during the auto-merge process.
getAutoMergingPreview_minAllowedConfidenceScoreForMerging :: Lens.Lens' GetAutoMergingPreview (Prelude.Maybe Prelude.Double)
getAutoMergingPreview_minAllowedConfidenceScoreForMerging = Lens.lens (\GetAutoMergingPreview' {minAllowedConfidenceScoreForMerging} -> minAllowedConfidenceScoreForMerging) (\s@GetAutoMergingPreview' {} a -> s {minAllowedConfidenceScoreForMerging = a} :: GetAutoMergingPreview)

-- | The unique name of the domain.
getAutoMergingPreview_domainName :: Lens.Lens' GetAutoMergingPreview Prelude.Text
getAutoMergingPreview_domainName = Lens.lens (\GetAutoMergingPreview' {domainName} -> domainName) (\s@GetAutoMergingPreview' {} a -> s {domainName = a} :: GetAutoMergingPreview)

-- | A list of matching attributes that represent matching criteria.
getAutoMergingPreview_consolidation :: Lens.Lens' GetAutoMergingPreview Consolidation
getAutoMergingPreview_consolidation = Lens.lens (\GetAutoMergingPreview' {consolidation} -> consolidation) (\s@GetAutoMergingPreview' {} a -> s {consolidation = a} :: GetAutoMergingPreview)

-- | How the auto-merging process should resolve conflicts between different
-- profiles.
getAutoMergingPreview_conflictResolution :: Lens.Lens' GetAutoMergingPreview ConflictResolution
getAutoMergingPreview_conflictResolution = Lens.lens (\GetAutoMergingPreview' {conflictResolution} -> conflictResolution) (\s@GetAutoMergingPreview' {} a -> s {conflictResolution = a} :: GetAutoMergingPreview)

instance Core.AWSRequest GetAutoMergingPreview where
  type
    AWSResponse GetAutoMergingPreview =
      GetAutoMergingPreviewResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAutoMergingPreviewResponse'
            Prelude.<$> (x Data..?> "NumberOfMatchesInSample")
            Prelude.<*> (x Data..?> "NumberOfProfilesInSample")
            Prelude.<*> (x Data..?> "NumberOfProfilesWillBeMerged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "DomainName")
      )

instance Prelude.Hashable GetAutoMergingPreview where
  hashWithSalt _salt GetAutoMergingPreview' {..} =
    _salt
      `Prelude.hashWithSalt` minAllowedConfidenceScoreForMerging
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` consolidation
      `Prelude.hashWithSalt` conflictResolution

instance Prelude.NFData GetAutoMergingPreview where
  rnf GetAutoMergingPreview' {..} =
    Prelude.rnf minAllowedConfidenceScoreForMerging
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf consolidation
      `Prelude.seq` Prelude.rnf conflictResolution

instance Data.ToHeaders GetAutoMergingPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAutoMergingPreview where
  toJSON GetAutoMergingPreview' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MinAllowedConfidenceScoreForMerging" Data..=)
              Prelude.<$> minAllowedConfidenceScoreForMerging,
            Prelude.Just ("Consolidation" Data..= consolidation),
            Prelude.Just
              ("ConflictResolution" Data..= conflictResolution)
          ]
      )

instance Data.ToPath GetAutoMergingPreview where
  toPath GetAutoMergingPreview' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/identity-resolution-jobs/auto-merging-preview"
      ]

instance Data.ToQuery GetAutoMergingPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAutoMergingPreviewResponse' smart constructor.
data GetAutoMergingPreviewResponse = GetAutoMergingPreviewResponse'
  { -- | The number of match groups in the domain that have been reviewed in this
    -- preview dry run.
    numberOfMatchesInSample :: Prelude.Maybe Prelude.Integer,
    -- | The number of profiles found in this preview dry run.
    numberOfProfilesInSample :: Prelude.Maybe Prelude.Integer,
    -- | The number of profiles that would be merged if this wasn\'t a preview
    -- dry run.
    numberOfProfilesWillBeMerged :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAutoMergingPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfMatchesInSample', 'getAutoMergingPreviewResponse_numberOfMatchesInSample' - The number of match groups in the domain that have been reviewed in this
-- preview dry run.
--
-- 'numberOfProfilesInSample', 'getAutoMergingPreviewResponse_numberOfProfilesInSample' - The number of profiles found in this preview dry run.
--
-- 'numberOfProfilesWillBeMerged', 'getAutoMergingPreviewResponse_numberOfProfilesWillBeMerged' - The number of profiles that would be merged if this wasn\'t a preview
-- dry run.
--
-- 'httpStatus', 'getAutoMergingPreviewResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'getAutoMergingPreviewResponse_domainName' - The unique name of the domain.
newGetAutoMergingPreviewResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  GetAutoMergingPreviewResponse
newGetAutoMergingPreviewResponse
  pHttpStatus_
  pDomainName_ =
    GetAutoMergingPreviewResponse'
      { numberOfMatchesInSample =
          Prelude.Nothing,
        numberOfProfilesInSample = Prelude.Nothing,
        numberOfProfilesWillBeMerged =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_
      }

-- | The number of match groups in the domain that have been reviewed in this
-- preview dry run.
getAutoMergingPreviewResponse_numberOfMatchesInSample :: Lens.Lens' GetAutoMergingPreviewResponse (Prelude.Maybe Prelude.Integer)
getAutoMergingPreviewResponse_numberOfMatchesInSample = Lens.lens (\GetAutoMergingPreviewResponse' {numberOfMatchesInSample} -> numberOfMatchesInSample) (\s@GetAutoMergingPreviewResponse' {} a -> s {numberOfMatchesInSample = a} :: GetAutoMergingPreviewResponse)

-- | The number of profiles found in this preview dry run.
getAutoMergingPreviewResponse_numberOfProfilesInSample :: Lens.Lens' GetAutoMergingPreviewResponse (Prelude.Maybe Prelude.Integer)
getAutoMergingPreviewResponse_numberOfProfilesInSample = Lens.lens (\GetAutoMergingPreviewResponse' {numberOfProfilesInSample} -> numberOfProfilesInSample) (\s@GetAutoMergingPreviewResponse' {} a -> s {numberOfProfilesInSample = a} :: GetAutoMergingPreviewResponse)

-- | The number of profiles that would be merged if this wasn\'t a preview
-- dry run.
getAutoMergingPreviewResponse_numberOfProfilesWillBeMerged :: Lens.Lens' GetAutoMergingPreviewResponse (Prelude.Maybe Prelude.Integer)
getAutoMergingPreviewResponse_numberOfProfilesWillBeMerged = Lens.lens (\GetAutoMergingPreviewResponse' {numberOfProfilesWillBeMerged} -> numberOfProfilesWillBeMerged) (\s@GetAutoMergingPreviewResponse' {} a -> s {numberOfProfilesWillBeMerged = a} :: GetAutoMergingPreviewResponse)

-- | The response's http status code.
getAutoMergingPreviewResponse_httpStatus :: Lens.Lens' GetAutoMergingPreviewResponse Prelude.Int
getAutoMergingPreviewResponse_httpStatus = Lens.lens (\GetAutoMergingPreviewResponse' {httpStatus} -> httpStatus) (\s@GetAutoMergingPreviewResponse' {} a -> s {httpStatus = a} :: GetAutoMergingPreviewResponse)

-- | The unique name of the domain.
getAutoMergingPreviewResponse_domainName :: Lens.Lens' GetAutoMergingPreviewResponse Prelude.Text
getAutoMergingPreviewResponse_domainName = Lens.lens (\GetAutoMergingPreviewResponse' {domainName} -> domainName) (\s@GetAutoMergingPreviewResponse' {} a -> s {domainName = a} :: GetAutoMergingPreviewResponse)

instance Prelude.NFData GetAutoMergingPreviewResponse where
  rnf GetAutoMergingPreviewResponse' {..} =
    Prelude.rnf numberOfMatchesInSample
      `Prelude.seq` Prelude.rnf numberOfProfilesInSample
      `Prelude.seq` Prelude.rnf numberOfProfilesWillBeMerged
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf domainName
