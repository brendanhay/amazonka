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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @BuildSuggester@ operation.
-- Specifies the name of the domain you want to update.
--
-- /See:/ 'newBuildSuggesters' smart constructor.
data BuildSuggesters = BuildSuggesters'
  { domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  BuildSuggesters
newBuildSuggesters pDomainName_ =
  BuildSuggesters' {domainName = pDomainName_}

-- | Undocumented member.
buildSuggesters_domainName :: Lens.Lens' BuildSuggesters Prelude.Text
buildSuggesters_domainName = Lens.lens (\BuildSuggesters' {domainName} -> domainName) (\s@BuildSuggesters' {} a -> s {domainName = a} :: BuildSuggesters)

instance Prelude.AWSRequest BuildSuggesters where
  type Rs BuildSuggesters = BuildSuggestersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "BuildSuggestersResult"
      ( \s h x ->
          BuildSuggestersResponse'
            Prelude.<$> ( x Prelude..@? "FieldNames"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BuildSuggesters

instance Prelude.NFData BuildSuggesters

instance Prelude.ToHeaders BuildSuggesters where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath BuildSuggesters where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BuildSuggesters where
  toQuery BuildSuggesters' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("BuildSuggesters" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName
      ]

-- | The result of a @BuildSuggester@ request. Contains a list of the fields
-- used for suggestions.
--
-- /See:/ 'newBuildSuggestersResponse' smart constructor.
data BuildSuggestersResponse = BuildSuggestersResponse'
  { fieldNames :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  BuildSuggestersResponse
newBuildSuggestersResponse pHttpStatus_ =
  BuildSuggestersResponse'
    { fieldNames =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
buildSuggestersResponse_fieldNames :: Lens.Lens' BuildSuggestersResponse (Prelude.Maybe [Prelude.Text])
buildSuggestersResponse_fieldNames = Lens.lens (\BuildSuggestersResponse' {fieldNames} -> fieldNames) (\s@BuildSuggestersResponse' {} a -> s {fieldNames = a} :: BuildSuggestersResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
buildSuggestersResponse_httpStatus :: Lens.Lens' BuildSuggestersResponse Prelude.Int
buildSuggestersResponse_httpStatus = Lens.lens (\BuildSuggestersResponse' {httpStatus} -> httpStatus) (\s@BuildSuggestersResponse' {} a -> s {httpStatus = a} :: BuildSuggestersResponse)

instance Prelude.NFData BuildSuggestersResponse
