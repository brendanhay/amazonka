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
-- Module      : Amazonka.RAM.GetResourceShareAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the resource and principal associations for resource shares
-- that you own.
--
-- This operation returns paginated results.
module Amazonka.RAM.GetResourceShareAssociations
  ( -- * Creating a Request
    GetResourceShareAssociations (..),
    newGetResourceShareAssociations,

    -- * Request Lenses
    getResourceShareAssociations_associationStatus,
    getResourceShareAssociations_maxResults,
    getResourceShareAssociations_nextToken,
    getResourceShareAssociations_principal,
    getResourceShareAssociations_resourceArn,
    getResourceShareAssociations_resourceShareArns,
    getResourceShareAssociations_associationType,

    -- * Destructuring the Response
    GetResourceShareAssociationsResponse (..),
    newGetResourceShareAssociationsResponse,

    -- * Response Lenses
    getResourceShareAssociationsResponse_nextToken,
    getResourceShareAssociationsResponse_resourceShareAssociations,
    getResourceShareAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceShareAssociations' smart constructor.
data GetResourceShareAssociations = GetResourceShareAssociations'
  { -- | Specifies that you want to retrieve only associations with this status.
    associationStatus :: Prelude.Maybe ResourceShareAssociationStatus,
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
    -- | Specifies the ID of the principal whose resource shares you want to
    -- retrieve. This can be an Amazon Web Services account ID, an organization
    -- ID, an organizational unit ID, or the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of an individual IAM user or role.
    --
    -- You cannot specify this parameter if the association type is @RESOURCE@.
    principal :: Prelude.Maybe Prelude.Text,
    -- | Specifies the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    -- of the resource whose resource shares you want to retrieve.
    --
    -- You cannot specify this parameter if the association type is
    -- @PRINCIPAL@.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies a list of
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- of the resource share whose associations you want to retrieve.
    resourceShareArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether you want to retrieve the associations that involve a
    -- specified resource or principal.
    --
    -- -   @PRINCIPAL@ – list the principals that are associated with the
    --     specified resource share.
    --
    -- -   @RESOURCE@ – list the resources that are associated with the
    --     specified resource share.
    associationType :: ResourceShareAssociationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceShareAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationStatus', 'getResourceShareAssociations_associationStatus' - Specifies that you want to retrieve only associations with this status.
--
-- 'maxResults', 'getResourceShareAssociations_maxResults' - Specifies the total number of results that you want included on each
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
-- 'nextToken', 'getResourceShareAssociations_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'principal', 'getResourceShareAssociations_principal' - Specifies the ID of the principal whose resource shares you want to
-- retrieve. This can be an Amazon Web Services account ID, an organization
-- ID, an organizational unit ID, or the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of an individual IAM user or role.
--
-- You cannot specify this parameter if the association type is @RESOURCE@.
--
-- 'resourceArn', 'getResourceShareAssociations_resourceArn' - Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource whose resource shares you want to retrieve.
--
-- You cannot specify this parameter if the association type is
-- @PRINCIPAL@.
--
-- 'resourceShareArns', 'getResourceShareAssociations_resourceShareArns' - Specifies a list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the resource share whose associations you want to retrieve.
--
-- 'associationType', 'getResourceShareAssociations_associationType' - Specifies whether you want to retrieve the associations that involve a
-- specified resource or principal.
--
-- -   @PRINCIPAL@ – list the principals that are associated with the
--     specified resource share.
--
-- -   @RESOURCE@ – list the resources that are associated with the
--     specified resource share.
newGetResourceShareAssociations ::
  -- | 'associationType'
  ResourceShareAssociationType ->
  GetResourceShareAssociations
newGetResourceShareAssociations pAssociationType_ =
  GetResourceShareAssociations'
    { associationStatus =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      principal = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing,
      associationType = pAssociationType_
    }

-- | Specifies that you want to retrieve only associations with this status.
getResourceShareAssociations_associationStatus :: Lens.Lens' GetResourceShareAssociations (Prelude.Maybe ResourceShareAssociationStatus)
getResourceShareAssociations_associationStatus = Lens.lens (\GetResourceShareAssociations' {associationStatus} -> associationStatus) (\s@GetResourceShareAssociations' {} a -> s {associationStatus = a} :: GetResourceShareAssociations)

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
getResourceShareAssociations_maxResults :: Lens.Lens' GetResourceShareAssociations (Prelude.Maybe Prelude.Natural)
getResourceShareAssociations_maxResults = Lens.lens (\GetResourceShareAssociations' {maxResults} -> maxResults) (\s@GetResourceShareAssociations' {} a -> s {maxResults = a} :: GetResourceShareAssociations)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
getResourceShareAssociations_nextToken :: Lens.Lens' GetResourceShareAssociations (Prelude.Maybe Prelude.Text)
getResourceShareAssociations_nextToken = Lens.lens (\GetResourceShareAssociations' {nextToken} -> nextToken) (\s@GetResourceShareAssociations' {} a -> s {nextToken = a} :: GetResourceShareAssociations)

-- | Specifies the ID of the principal whose resource shares you want to
-- retrieve. This can be an Amazon Web Services account ID, an organization
-- ID, an organizational unit ID, or the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of an individual IAM user or role.
--
-- You cannot specify this parameter if the association type is @RESOURCE@.
getResourceShareAssociations_principal :: Lens.Lens' GetResourceShareAssociations (Prelude.Maybe Prelude.Text)
getResourceShareAssociations_principal = Lens.lens (\GetResourceShareAssociations' {principal} -> principal) (\s@GetResourceShareAssociations' {} a -> s {principal = a} :: GetResourceShareAssociations)

-- | Specifies the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
-- of the resource whose resource shares you want to retrieve.
--
-- You cannot specify this parameter if the association type is
-- @PRINCIPAL@.
getResourceShareAssociations_resourceArn :: Lens.Lens' GetResourceShareAssociations (Prelude.Maybe Prelude.Text)
getResourceShareAssociations_resourceArn = Lens.lens (\GetResourceShareAssociations' {resourceArn} -> resourceArn) (\s@GetResourceShareAssociations' {} a -> s {resourceArn = a} :: GetResourceShareAssociations)

-- | Specifies a list of
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- of the resource share whose associations you want to retrieve.
getResourceShareAssociations_resourceShareArns :: Lens.Lens' GetResourceShareAssociations (Prelude.Maybe [Prelude.Text])
getResourceShareAssociations_resourceShareArns = Lens.lens (\GetResourceShareAssociations' {resourceShareArns} -> resourceShareArns) (\s@GetResourceShareAssociations' {} a -> s {resourceShareArns = a} :: GetResourceShareAssociations) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether you want to retrieve the associations that involve a
-- specified resource or principal.
--
-- -   @PRINCIPAL@ – list the principals that are associated with the
--     specified resource share.
--
-- -   @RESOURCE@ – list the resources that are associated with the
--     specified resource share.
getResourceShareAssociations_associationType :: Lens.Lens' GetResourceShareAssociations ResourceShareAssociationType
getResourceShareAssociations_associationType = Lens.lens (\GetResourceShareAssociations' {associationType} -> associationType) (\s@GetResourceShareAssociations' {} a -> s {associationType = a} :: GetResourceShareAssociations)

instance Core.AWSPager GetResourceShareAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getResourceShareAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getResourceShareAssociationsResponse_resourceShareAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getResourceShareAssociations_nextToken
          Lens..~ rs
          Lens.^? getResourceShareAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetResourceShareAssociations where
  type
    AWSResponse GetResourceShareAssociations =
      GetResourceShareAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceShareAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "resourceShareAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResourceShareAssociations
  where
  hashWithSalt _salt GetResourceShareAssociations' {..} =
    _salt
      `Prelude.hashWithSalt` associationStatus
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceShareArns
      `Prelude.hashWithSalt` associationType

instance Prelude.NFData GetResourceShareAssociations where
  rnf GetResourceShareAssociations' {..} =
    Prelude.rnf associationStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principal
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceShareArns
      `Prelude.seq` Prelude.rnf associationType

instance Data.ToHeaders GetResourceShareAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceShareAssociations where
  toJSON GetResourceShareAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associationStatus" Data..=)
              Prelude.<$> associationStatus,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("principal" Data..=) Prelude.<$> principal,
            ("resourceArn" Data..=) Prelude.<$> resourceArn,
            ("resourceShareArns" Data..=)
              Prelude.<$> resourceShareArns,
            Prelude.Just
              ("associationType" Data..= associationType)
          ]
      )

instance Data.ToPath GetResourceShareAssociations where
  toPath =
    Prelude.const "/getresourceshareassociations"

instance Data.ToQuery GetResourceShareAssociations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceShareAssociationsResponse' smart constructor.
data GetResourceShareAssociationsResponse = GetResourceShareAssociationsResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain the details about the associations.
    resourceShareAssociations :: Prelude.Maybe [ResourceShareAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceShareAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getResourceShareAssociationsResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'resourceShareAssociations', 'getResourceShareAssociationsResponse_resourceShareAssociations' - An array of objects that contain the details about the associations.
--
-- 'httpStatus', 'getResourceShareAssociationsResponse_httpStatus' - The response's http status code.
newGetResourceShareAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceShareAssociationsResponse
newGetResourceShareAssociationsResponse pHttpStatus_ =
  GetResourceShareAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      resourceShareAssociations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
getResourceShareAssociationsResponse_nextToken :: Lens.Lens' GetResourceShareAssociationsResponse (Prelude.Maybe Prelude.Text)
getResourceShareAssociationsResponse_nextToken = Lens.lens (\GetResourceShareAssociationsResponse' {nextToken} -> nextToken) (\s@GetResourceShareAssociationsResponse' {} a -> s {nextToken = a} :: GetResourceShareAssociationsResponse)

-- | An array of objects that contain the details about the associations.
getResourceShareAssociationsResponse_resourceShareAssociations :: Lens.Lens' GetResourceShareAssociationsResponse (Prelude.Maybe [ResourceShareAssociation])
getResourceShareAssociationsResponse_resourceShareAssociations = Lens.lens (\GetResourceShareAssociationsResponse' {resourceShareAssociations} -> resourceShareAssociations) (\s@GetResourceShareAssociationsResponse' {} a -> s {resourceShareAssociations = a} :: GetResourceShareAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourceShareAssociationsResponse_httpStatus :: Lens.Lens' GetResourceShareAssociationsResponse Prelude.Int
getResourceShareAssociationsResponse_httpStatus = Lens.lens (\GetResourceShareAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetResourceShareAssociationsResponse' {} a -> s {httpStatus = a} :: GetResourceShareAssociationsResponse)

instance
  Prelude.NFData
    GetResourceShareAssociationsResponse
  where
  rnf GetResourceShareAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resourceShareAssociations
      `Prelude.seq` Prelude.rnf httpStatus
