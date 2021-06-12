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
-- Module      : Network.AWS.CloudSearch.BuildSuggesters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Indexes the search suggestions. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters Configuring Suggesters>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.BuildSuggesters
  ( -- * Creating a Request
    BuildSuggesters (..),
    newBuildSuggesters,

    -- * Request Lenses
    buildSuggesters_domainName,

    -- * Destructuring the Response
    BuildSuggestersResponse (..),
    newBuildSuggestersResponse,

    -- * Response Lenses
    buildSuggestersResponse_fieldNames,
    buildSuggestersResponse_httpStatus,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @BuildSuggester@ operation.
-- Specifies the name of the domain you want to update.
--
-- /See:/ 'newBuildSuggesters' smart constructor.
data BuildSuggesters = BuildSuggesters'
  { domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildSuggesters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'buildSuggesters_domainName' - Undocumented member.
newBuildSuggesters ::
  -- | 'domainName'
  Core.Text ->
  BuildSuggesters
newBuildSuggesters pDomainName_ =
  BuildSuggesters' {domainName = pDomainName_}

-- | Undocumented member.
buildSuggesters_domainName :: Lens.Lens' BuildSuggesters Core.Text
buildSuggesters_domainName = Lens.lens (\BuildSuggesters' {domainName} -> domainName) (\s@BuildSuggesters' {} a -> s {domainName = a} :: BuildSuggesters)

instance Core.AWSRequest BuildSuggesters where
  type
    AWSResponse BuildSuggesters =
      BuildSuggestersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BuildSuggestersResult"
      ( \s h x ->
          BuildSuggestersResponse'
            Core.<$> ( x Core..@? "FieldNames" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BuildSuggesters

instance Core.NFData BuildSuggesters

instance Core.ToHeaders BuildSuggesters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BuildSuggesters where
  toPath = Core.const "/"

instance Core.ToQuery BuildSuggesters where
  toQuery BuildSuggesters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BuildSuggesters" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @BuildSuggester@ request. Contains a list of the fields
-- used for suggestions.
--
-- /See:/ 'newBuildSuggestersResponse' smart constructor.
data BuildSuggestersResponse = BuildSuggestersResponse'
  { fieldNames :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildSuggestersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldNames', 'buildSuggestersResponse_fieldNames' - Undocumented member.
--
-- 'httpStatus', 'buildSuggestersResponse_httpStatus' - The response's http status code.
newBuildSuggestersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BuildSuggestersResponse
newBuildSuggestersResponse pHttpStatus_ =
  BuildSuggestersResponse'
    { fieldNames = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
buildSuggestersResponse_fieldNames :: Lens.Lens' BuildSuggestersResponse (Core.Maybe [Core.Text])
buildSuggestersResponse_fieldNames = Lens.lens (\BuildSuggestersResponse' {fieldNames} -> fieldNames) (\s@BuildSuggestersResponse' {} a -> s {fieldNames = a} :: BuildSuggestersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
buildSuggestersResponse_httpStatus :: Lens.Lens' BuildSuggestersResponse Core.Int
buildSuggestersResponse_httpStatus = Lens.lens (\BuildSuggestersResponse' {httpStatus} -> httpStatus) (\s@BuildSuggestersResponse' {} a -> s {httpStatus = a} :: BuildSuggestersResponse)

instance Core.NFData BuildSuggestersResponse
