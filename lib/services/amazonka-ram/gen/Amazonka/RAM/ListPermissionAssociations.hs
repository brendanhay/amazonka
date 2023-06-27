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
-- Module      : Amazonka.RAM.ListPermissionAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the managed permission and its associations to
-- any resource shares that use this managed permission. This lets you see
-- which resource shares use which versions of the specified managed
-- permission.
module Amazonka.RAM.ListPermissionAssociations
  ( -- * Creating a Request
    ListPermissionAssociations (..),
    newListPermissionAssociations,

    -- * Request Lenses
    listPermissionAssociations_associationStatus,
    listPermissionAssociations_defaultVersion,
    listPermissionAssociations_featureSet,
    listPermissionAssociations_maxResults,
    listPermissionAssociations_nextToken,
    listPermissionAssociations_permissionArn,
    listPermissionAssociations_permissionVersion,
    listPermissionAssociations_resourceType,

    -- * Destructuring the Response
    ListPermissionAssociationsResponse (..),
    newListPermissionAssociationsResponse,

    -- * Response Lenses
    listPermissionAssociationsResponse_nextToken,
    listPermissionAssociationsResponse_permissions,
    listPermissionAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPermissionAssociations' smart constructor.
data ListPermissionAssociations = ListPermissionAssociations'
  { -- | Specifies that you want to list only those associations with resource
    -- shares that match this status.
    associationStatus :: Prelude.Maybe ResourceShareAssociationStatus,
    -- | When @true@, specifies that you want to list only those associations
    -- with resource shares that use the default version of the specified
    -- managed permission.
    --
    -- When @false@ (the default value), lists associations with resource
    -- shares that use any version of the specified managed permission.
    defaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that you want to list only those associations with resource
    -- shares that have a @featureSet@ with this value.
    featureSet :: Prelude.Maybe PermissionFeatureSet,
    -- | Specifies the total number of results that you want included on each
    -- page of the response. If you do not include this parameter, it defaults
    -- to a value that is specific to the operation. If additional items exist
    -- beyond the number you specify, the @NextToken@ response element is
    -- returned with a value (not null). Include the specified value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results. Note that the service might return fewer
    -- results than the maximum even when there are more results available. You
    -- should check @NextToken@ after every operation to ensure that you
    -- receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
    -- of the managed permission.
    permissionArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to list only those associations with resource
    -- shares that use this version of the managed permission. If you don\'t
    -- provide a value for this parameter, then the operation returns
    -- information about associations with resource shares that use any version
    -- of the managed permission.
    permissionVersion :: Prelude.Maybe Prelude.Int,
    -- | Specifies that you want to list only those associations with resource
    -- shares that include at least one resource of this resource type.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationStatus', 'listPermissionAssociations_associationStatus' - Specifies that you want to list only those associations with resource
-- shares that match this status.
--
-- 'defaultVersion', 'listPermissionAssociations_defaultVersion' - When @true@, specifies that you want to list only those associations
-- with resource shares that use the default version of the specified
-- managed permission.
--
-- When @false@ (the default value), lists associations with resource
-- shares that use any version of the specified managed permission.
--
-- 'featureSet', 'listPermissionAssociations_featureSet' - Specifies that you want to list only those associations with resource
-- shares that have a @featureSet@ with this value.
--
-- 'maxResults', 'listPermissionAssociations_maxResults' - Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
--
-- 'nextToken', 'listPermissionAssociations_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'permissionArn', 'listPermissionAssociations_permissionArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the managed permission.
--
-- 'permissionVersion', 'listPermissionAssociations_permissionVersion' - Specifies that you want to list only those associations with resource
-- shares that use this version of the managed permission. If you don\'t
-- provide a value for this parameter, then the operation returns
-- information about associations with resource shares that use any version
-- of the managed permission.
--
-- 'resourceType', 'listPermissionAssociations_resourceType' - Specifies that you want to list only those associations with resource
-- shares that include at least one resource of this resource type.
newListPermissionAssociations ::
  ListPermissionAssociations
newListPermissionAssociations =
  ListPermissionAssociations'
    { associationStatus =
        Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      featureSet = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      permissionArn = Prelude.Nothing,
      permissionVersion = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Specifies that you want to list only those associations with resource
-- shares that match this status.
listPermissionAssociations_associationStatus :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe ResourceShareAssociationStatus)
listPermissionAssociations_associationStatus = Lens.lens (\ListPermissionAssociations' {associationStatus} -> associationStatus) (\s@ListPermissionAssociations' {} a -> s {associationStatus = a} :: ListPermissionAssociations)

-- | When @true@, specifies that you want to list only those associations
-- with resource shares that use the default version of the specified
-- managed permission.
--
-- When @false@ (the default value), lists associations with resource
-- shares that use any version of the specified managed permission.
listPermissionAssociations_defaultVersion :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe Prelude.Bool)
listPermissionAssociations_defaultVersion = Lens.lens (\ListPermissionAssociations' {defaultVersion} -> defaultVersion) (\s@ListPermissionAssociations' {} a -> s {defaultVersion = a} :: ListPermissionAssociations)

-- | Specifies that you want to list only those associations with resource
-- shares that have a @featureSet@ with this value.
listPermissionAssociations_featureSet :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe PermissionFeatureSet)
listPermissionAssociations_featureSet = Lens.lens (\ListPermissionAssociations' {featureSet} -> featureSet) (\s@ListPermissionAssociations' {} a -> s {featureSet = a} :: ListPermissionAssociations)

-- | Specifies the total number of results that you want included on each
-- page of the response. If you do not include this parameter, it defaults
-- to a value that is specific to the operation. If additional items exist
-- beyond the number you specify, the @NextToken@ response element is
-- returned with a value (not null). Include the specified value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results. Note that the service might return fewer
-- results than the maximum even when there are more results available. You
-- should check @NextToken@ after every operation to ensure that you
-- receive all of the results.
listPermissionAssociations_maxResults :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe Prelude.Natural)
listPermissionAssociations_maxResults = Lens.lens (\ListPermissionAssociations' {maxResults} -> maxResults) (\s@ListPermissionAssociations' {} a -> s {maxResults = a} :: ListPermissionAssociations)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listPermissionAssociations_nextToken :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe Prelude.Text)
listPermissionAssociations_nextToken = Lens.lens (\ListPermissionAssociations' {nextToken} -> nextToken) (\s@ListPermissionAssociations' {} a -> s {nextToken = a} :: ListPermissionAssociations)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Name (ARN)>
-- of the managed permission.
listPermissionAssociations_permissionArn :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe Prelude.Text)
listPermissionAssociations_permissionArn = Lens.lens (\ListPermissionAssociations' {permissionArn} -> permissionArn) (\s@ListPermissionAssociations' {} a -> s {permissionArn = a} :: ListPermissionAssociations)

-- | Specifies that you want to list only those associations with resource
-- shares that use this version of the managed permission. If you don\'t
-- provide a value for this parameter, then the operation returns
-- information about associations with resource shares that use any version
-- of the managed permission.
listPermissionAssociations_permissionVersion :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe Prelude.Int)
listPermissionAssociations_permissionVersion = Lens.lens (\ListPermissionAssociations' {permissionVersion} -> permissionVersion) (\s@ListPermissionAssociations' {} a -> s {permissionVersion = a} :: ListPermissionAssociations)

-- | Specifies that you want to list only those associations with resource
-- shares that include at least one resource of this resource type.
listPermissionAssociations_resourceType :: Lens.Lens' ListPermissionAssociations (Prelude.Maybe Prelude.Text)
listPermissionAssociations_resourceType = Lens.lens (\ListPermissionAssociations' {resourceType} -> resourceType) (\s@ListPermissionAssociations' {} a -> s {resourceType = a} :: ListPermissionAssociations)

instance Core.AWSRequest ListPermissionAssociations where
  type
    AWSResponse ListPermissionAssociations =
      ListPermissionAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPermissionAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "permissions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPermissionAssociations where
  hashWithSalt _salt ListPermissionAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` associationStatus
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` featureSet
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` permissionArn
      `Prelude.hashWithSalt` permissionVersion
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ListPermissionAssociations where
  rnf ListPermissionAssociations' {..} =
    Prelude.rnf associationStatus
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf featureSet
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissionArn
      `Prelude.seq` Prelude.rnf permissionVersion
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders ListPermissionAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPermissionAssociations where
  toJSON ListPermissionAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associationStatus" Data..=)
              Prelude.<$> associationStatus,
            ("defaultVersion" Data..=)
              Prelude.<$> defaultVersion,
            ("featureSet" Data..=) Prelude.<$> featureSet,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("permissionArn" Data..=) Prelude.<$> permissionArn,
            ("permissionVersion" Data..=)
              Prelude.<$> permissionVersion,
            ("resourceType" Data..=) Prelude.<$> resourceType
          ]
      )

instance Data.ToPath ListPermissionAssociations where
  toPath = Prelude.const "/listpermissionassociations"

instance Data.ToQuery ListPermissionAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPermissionAssociationsResponse' smart constructor.
data ListPermissionAssociationsResponse = ListPermissionAssociationsResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure with information about this customer managed permission.
    permissions :: Prelude.Maybe [AssociatedPermission],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPermissionAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPermissionAssociationsResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'permissions', 'listPermissionAssociationsResponse_permissions' - A structure with information about this customer managed permission.
--
-- 'httpStatus', 'listPermissionAssociationsResponse_httpStatus' - The response's http status code.
newListPermissionAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPermissionAssociationsResponse
newListPermissionAssociationsResponse pHttpStatus_ =
  ListPermissionAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      permissions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listPermissionAssociationsResponse_nextToken :: Lens.Lens' ListPermissionAssociationsResponse (Prelude.Maybe Prelude.Text)
listPermissionAssociationsResponse_nextToken = Lens.lens (\ListPermissionAssociationsResponse' {nextToken} -> nextToken) (\s@ListPermissionAssociationsResponse' {} a -> s {nextToken = a} :: ListPermissionAssociationsResponse)

-- | A structure with information about this customer managed permission.
listPermissionAssociationsResponse_permissions :: Lens.Lens' ListPermissionAssociationsResponse (Prelude.Maybe [AssociatedPermission])
listPermissionAssociationsResponse_permissions = Lens.lens (\ListPermissionAssociationsResponse' {permissions} -> permissions) (\s@ListPermissionAssociationsResponse' {} a -> s {permissions = a} :: ListPermissionAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPermissionAssociationsResponse_httpStatus :: Lens.Lens' ListPermissionAssociationsResponse Prelude.Int
listPermissionAssociationsResponse_httpStatus = Lens.lens (\ListPermissionAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListPermissionAssociationsResponse' {} a -> s {httpStatus = a} :: ListPermissionAssociationsResponse)

instance
  Prelude.NFData
    ListPermissionAssociationsResponse
  where
  rnf ListPermissionAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf permissions
      `Prelude.seq` Prelude.rnf httpStatus
