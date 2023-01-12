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
-- Module      : Amazonka.CloudFront.ListConflictingAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of aliases (also called CNAMEs or alternate domain names)
-- that conflict or overlap with the provided alias, and the associated
-- CloudFront distributions and Amazon Web Services accounts for each
-- conflicting alias. In the returned list, the distribution and account
-- IDs are partially hidden, which allows you to identify the distributions
-- and accounts that you own, but helps to protect the information of ones
-- that you don\'t own.
--
-- Use this operation to find aliases that are in use in CloudFront that
-- conflict or overlap with the provided alias. For example, if you provide
-- @www.example.com@ as input, the returned list can include
-- @www.example.com@ and the overlapping wildcard alternate domain name
-- (@*.example.com@), if they exist. If you provide @*.example.com@ as
-- input, the returned list can include @*.example.com@ and any alternate
-- domain names covered by that wildcard (for example, @www.example.com@,
-- @test.example.com@, @dev.example.com@, and so on), if they exist.
--
-- To list conflicting aliases, you provide the alias to search and the ID
-- of a distribution in your account that has an attached SSL\/TLS
-- certificate that includes the provided alias. For more information,
-- including how to set up the distribution and certificate, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/CNAMEs.html#alternate-domain-names-move Moving an alternate domain name to a different distribution>
-- in the /Amazon CloudFront Developer Guide/.
--
-- You can optionally specify the maximum number of items to receive in the
-- response. If the total number of items in the list exceeds the maximum
-- that you specify, or the default maximum, the response is paginated. To
-- get the next page of items, send a subsequent request that specifies the
-- @NextMarker@ value from the current response as the @Marker@ value in
-- the subsequent request.
module Amazonka.CloudFront.ListConflictingAliases
  ( -- * Creating a Request
    ListConflictingAliases (..),
    newListConflictingAliases,

    -- * Request Lenses
    listConflictingAliases_marker,
    listConflictingAliases_maxItems,
    listConflictingAliases_distributionId,
    listConflictingAliases_alias,

    -- * Destructuring the Response
    ListConflictingAliasesResponse (..),
    newListConflictingAliasesResponse,

    -- * Response Lenses
    listConflictingAliasesResponse_conflictingAliasesList,
    listConflictingAliasesResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConflictingAliases' smart constructor.
data ListConflictingAliases = ListConflictingAliases'
  { -- | Use this field when paginating results to indicate where to begin in the
    -- list of conflicting aliases. The response includes conflicting aliases
    -- in the list that occur after the marker. To get the next page of the
    -- list, set this field\'s value to the value of @NextMarker@ from the
    -- current page\'s response.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of conflicting aliases that you want in the response.
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | The ID of a distribution in your account that has an attached SSL\/TLS
    -- certificate that includes the provided alias.
    distributionId :: Prelude.Text,
    -- | The alias (also called a CNAME) to search for conflicting aliases.
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConflictingAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listConflictingAliases_marker' - Use this field when paginating results to indicate where to begin in the
-- list of conflicting aliases. The response includes conflicting aliases
-- in the list that occur after the marker. To get the next page of the
-- list, set this field\'s value to the value of @NextMarker@ from the
-- current page\'s response.
--
-- 'maxItems', 'listConflictingAliases_maxItems' - The maximum number of conflicting aliases that you want in the response.
--
-- 'distributionId', 'listConflictingAliases_distributionId' - The ID of a distribution in your account that has an attached SSL\/TLS
-- certificate that includes the provided alias.
--
-- 'alias', 'listConflictingAliases_alias' - The alias (also called a CNAME) to search for conflicting aliases.
newListConflictingAliases ::
  -- | 'distributionId'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  ListConflictingAliases
newListConflictingAliases pDistributionId_ pAlias_ =
  ListConflictingAliases'
    { marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      distributionId = pDistributionId_,
      alias = pAlias_
    }

-- | Use this field when paginating results to indicate where to begin in the
-- list of conflicting aliases. The response includes conflicting aliases
-- in the list that occur after the marker. To get the next page of the
-- list, set this field\'s value to the value of @NextMarker@ from the
-- current page\'s response.
listConflictingAliases_marker :: Lens.Lens' ListConflictingAliases (Prelude.Maybe Prelude.Text)
listConflictingAliases_marker = Lens.lens (\ListConflictingAliases' {marker} -> marker) (\s@ListConflictingAliases' {} a -> s {marker = a} :: ListConflictingAliases)

-- | The maximum number of conflicting aliases that you want in the response.
listConflictingAliases_maxItems :: Lens.Lens' ListConflictingAliases (Prelude.Maybe Prelude.Int)
listConflictingAliases_maxItems = Lens.lens (\ListConflictingAliases' {maxItems} -> maxItems) (\s@ListConflictingAliases' {} a -> s {maxItems = a} :: ListConflictingAliases)

-- | The ID of a distribution in your account that has an attached SSL\/TLS
-- certificate that includes the provided alias.
listConflictingAliases_distributionId :: Lens.Lens' ListConflictingAliases Prelude.Text
listConflictingAliases_distributionId = Lens.lens (\ListConflictingAliases' {distributionId} -> distributionId) (\s@ListConflictingAliases' {} a -> s {distributionId = a} :: ListConflictingAliases)

-- | The alias (also called a CNAME) to search for conflicting aliases.
listConflictingAliases_alias :: Lens.Lens' ListConflictingAliases Prelude.Text
listConflictingAliases_alias = Lens.lens (\ListConflictingAliases' {alias} -> alias) (\s@ListConflictingAliases' {} a -> s {alias = a} :: ListConflictingAliases)

instance Core.AWSRequest ListConflictingAliases where
  type
    AWSResponse ListConflictingAliases =
      ListConflictingAliasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListConflictingAliasesResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListConflictingAliases where
  hashWithSalt _salt ListConflictingAliases' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` distributionId
      `Prelude.hashWithSalt` alias

instance Prelude.NFData ListConflictingAliases where
  rnf ListConflictingAliases' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf distributionId
      `Prelude.seq` Prelude.rnf alias

instance Data.ToHeaders ListConflictingAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListConflictingAliases where
  toPath =
    Prelude.const "/2020-05-31/conflicting-alias"

instance Data.ToQuery ListConflictingAliases where
  toQuery ListConflictingAliases' {..} =
    Prelude.mconcat
      [ "Marker" Data.=: marker,
        "MaxItems" Data.=: maxItems,
        "DistributionId" Data.=: distributionId,
        "Alias" Data.=: alias
      ]

-- | /See:/ 'newListConflictingAliasesResponse' smart constructor.
data ListConflictingAliasesResponse = ListConflictingAliasesResponse'
  { -- | A list of conflicting aliases.
    conflictingAliasesList :: Prelude.Maybe ConflictingAliasesList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConflictingAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conflictingAliasesList', 'listConflictingAliasesResponse_conflictingAliasesList' - A list of conflicting aliases.
--
-- 'httpStatus', 'listConflictingAliasesResponse_httpStatus' - The response's http status code.
newListConflictingAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConflictingAliasesResponse
newListConflictingAliasesResponse pHttpStatus_ =
  ListConflictingAliasesResponse'
    { conflictingAliasesList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of conflicting aliases.
listConflictingAliasesResponse_conflictingAliasesList :: Lens.Lens' ListConflictingAliasesResponse (Prelude.Maybe ConflictingAliasesList)
listConflictingAliasesResponse_conflictingAliasesList = Lens.lens (\ListConflictingAliasesResponse' {conflictingAliasesList} -> conflictingAliasesList) (\s@ListConflictingAliasesResponse' {} a -> s {conflictingAliasesList = a} :: ListConflictingAliasesResponse)

-- | The response's http status code.
listConflictingAliasesResponse_httpStatus :: Lens.Lens' ListConflictingAliasesResponse Prelude.Int
listConflictingAliasesResponse_httpStatus = Lens.lens (\ListConflictingAliasesResponse' {httpStatus} -> httpStatus) (\s@ListConflictingAliasesResponse' {} a -> s {httpStatus = a} :: ListConflictingAliasesResponse)

instance
  Prelude.NFData
    ListConflictingAliasesResponse
  where
  rnf ListConflictingAliasesResponse' {..} =
    Prelude.rnf conflictingAliasesList
      `Prelude.seq` Prelude.rnf httpStatus
