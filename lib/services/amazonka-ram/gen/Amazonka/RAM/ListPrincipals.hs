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
-- Module      : Amazonka.RAM.ListPrincipals
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the principals that you are sharing resources with or that are
-- sharing resources with you.
--
-- This operation returns paginated results.
module Amazonka.RAM.ListPrincipals
  ( -- * Creating a Request
    ListPrincipals (..),
    newListPrincipals,

    -- * Request Lenses
    listPrincipals_resourceType,
    listPrincipals_nextToken,
    listPrincipals_principals,
    listPrincipals_maxResults,
    listPrincipals_resourceShareArns,
    listPrincipals_resourceArn,
    listPrincipals_resourceOwner,

    -- * Destructuring the Response
    ListPrincipalsResponse (..),
    newListPrincipalsResponse,

    -- * Response Lenses
    listPrincipalsResponse_nextToken,
    listPrincipalsResponse_principals,
    listPrincipalsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RAM.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPrincipals' smart constructor.
data ListPrincipals = ListPrincipals'
  { -- | Specifies that you want to list information for only principals
    -- associated with resource shares that include the specified resource
    -- type.
    --
    -- For a list of valid values, query the ListResourceTypes operation.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to receive the next page of results. Valid only
    -- if you received a @NextToken@ response in the previous request. If you
    -- did, it indicates that more output is available. Set this parameter to
    -- the value provided by the previous call\'s @NextToken@ response to
    -- request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to list information for only the listed
    -- principals.
    --
    -- You can include the following values:
    --
    -- -   An Amazon Web Services account ID, for example: @123456789012@
    --
    -- -   An
    --     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
    --     of an organization in Organizations, for example:
    --     @organizations::123456789012:organization\/o-exampleorgid@
    --
    -- -   An ARN of an organizational unit (OU) in Organizations, for example:
    --     @organizations::123456789012:ou\/o-exampleorgid\/ou-examplerootid-exampleouid123@
    --
    -- -   An ARN of an IAM role, for example:
    --     @iam::123456789012:role\/rolename@
    --
    -- -   An ARN of an IAM user, for example:
    --     @iam::123456789012user\/username@
    --
    -- Not all resource types can be shared with IAM roles and users. For more
    -- information, see
    -- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and users>
    -- in the /Resource Access Manager User Guide/.
    principals :: Prelude.Maybe [Prelude.Text],
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
    -- | Specifies that you want to list information for only principals
    -- associated with the resource shares specified by a list the
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    resourceShareArns :: Prelude.Maybe [Prelude.Text],
    -- | Specifies that you want to list principal information for the resource
    -- share with the specified
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies that you want to list information for only resource shares
    -- that match the following:
    --
    -- -   __@SELF@__ – principals that your account is sharing resources with
    --
    -- -   __@OTHER-ACCOUNTS@__ – principals that are sharing resources with
    --     your account
    resourceOwner :: ResourceOwner
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'listPrincipals_resourceType' - Specifies that you want to list information for only principals
-- associated with resource shares that include the specified resource
-- type.
--
-- For a list of valid values, query the ListResourceTypes operation.
--
-- 'nextToken', 'listPrincipals_nextToken' - Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
--
-- 'principals', 'listPrincipals_principals' - Specifies that you want to list information for only the listed
-- principals.
--
-- You can include the following values:
--
-- -   An Amazon Web Services account ID, for example: @123456789012@
--
-- -   An
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--     of an organization in Organizations, for example:
--     @organizations::123456789012:organization\/o-exampleorgid@
--
-- -   An ARN of an organizational unit (OU) in Organizations, for example:
--     @organizations::123456789012:ou\/o-exampleorgid\/ou-examplerootid-exampleouid123@
--
-- -   An ARN of an IAM role, for example:
--     @iam::123456789012:role\/rolename@
--
-- -   An ARN of an IAM user, for example:
--     @iam::123456789012user\/username@
--
-- Not all resource types can be shared with IAM roles and users. For more
-- information, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and users>
-- in the /Resource Access Manager User Guide/.
--
-- 'maxResults', 'listPrincipals_maxResults' - Specifies the total number of results that you want included on each
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
-- 'resourceShareArns', 'listPrincipals_resourceShareArns' - Specifies that you want to list information for only principals
-- associated with the resource shares specified by a list the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- 'resourceArn', 'listPrincipals_resourceArn' - Specifies that you want to list principal information for the resource
-- share with the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>.
--
-- 'resourceOwner', 'listPrincipals_resourceOwner' - Specifies that you want to list information for only resource shares
-- that match the following:
--
-- -   __@SELF@__ – principals that your account is sharing resources with
--
-- -   __@OTHER-ACCOUNTS@__ – principals that are sharing resources with
--     your account
newListPrincipals ::
  -- | 'resourceOwner'
  ResourceOwner ->
  ListPrincipals
newListPrincipals pResourceOwner_ =
  ListPrincipals'
    { resourceType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      principals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceShareArns = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceOwner = pResourceOwner_
    }

-- | Specifies that you want to list information for only principals
-- associated with resource shares that include the specified resource
-- type.
--
-- For a list of valid values, query the ListResourceTypes operation.
listPrincipals_resourceType :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Text)
listPrincipals_resourceType = Lens.lens (\ListPrincipals' {resourceType} -> resourceType) (\s@ListPrincipals' {} a -> s {resourceType = a} :: ListPrincipals)

-- | Specifies that you want to receive the next page of results. Valid only
-- if you received a @NextToken@ response in the previous request. If you
-- did, it indicates that more output is available. Set this parameter to
-- the value provided by the previous call\'s @NextToken@ response to
-- request the next page of results.
listPrincipals_nextToken :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Text)
listPrincipals_nextToken = Lens.lens (\ListPrincipals' {nextToken} -> nextToken) (\s@ListPrincipals' {} a -> s {nextToken = a} :: ListPrincipals)

-- | Specifies that you want to list information for only the listed
-- principals.
--
-- You can include the following values:
--
-- -   An Amazon Web Services account ID, for example: @123456789012@
--
-- -   An
--     <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>
--     of an organization in Organizations, for example:
--     @organizations::123456789012:organization\/o-exampleorgid@
--
-- -   An ARN of an organizational unit (OU) in Organizations, for example:
--     @organizations::123456789012:ou\/o-exampleorgid\/ou-examplerootid-exampleouid123@
--
-- -   An ARN of an IAM role, for example:
--     @iam::123456789012:role\/rolename@
--
-- -   An ARN of an IAM user, for example:
--     @iam::123456789012user\/username@
--
-- Not all resource types can be shared with IAM roles and users. For more
-- information, see
-- <https://docs.aws.amazon.com/ram/latest/userguide/permissions.html#permissions-rbp-supported-resource-types Sharing with IAM roles and users>
-- in the /Resource Access Manager User Guide/.
listPrincipals_principals :: Lens.Lens' ListPrincipals (Prelude.Maybe [Prelude.Text])
listPrincipals_principals = Lens.lens (\ListPrincipals' {principals} -> principals) (\s@ListPrincipals' {} a -> s {principals = a} :: ListPrincipals) Prelude.. Lens.mapping Lens.coerced

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
listPrincipals_maxResults :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Natural)
listPrincipals_maxResults = Lens.lens (\ListPrincipals' {maxResults} -> maxResults) (\s@ListPrincipals' {} a -> s {maxResults = a} :: ListPrincipals)

-- | Specifies that you want to list information for only principals
-- associated with the resource shares specified by a list the
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
listPrincipals_resourceShareArns :: Lens.Lens' ListPrincipals (Prelude.Maybe [Prelude.Text])
listPrincipals_resourceShareArns = Lens.lens (\ListPrincipals' {resourceShareArns} -> resourceShareArns) (\s@ListPrincipals' {} a -> s {resourceShareArns = a} :: ListPrincipals) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that you want to list principal information for the resource
-- share with the specified
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resoure Name (ARN)>.
listPrincipals_resourceArn :: Lens.Lens' ListPrincipals (Prelude.Maybe Prelude.Text)
listPrincipals_resourceArn = Lens.lens (\ListPrincipals' {resourceArn} -> resourceArn) (\s@ListPrincipals' {} a -> s {resourceArn = a} :: ListPrincipals)

-- | Specifies that you want to list information for only resource shares
-- that match the following:
--
-- -   __@SELF@__ – principals that your account is sharing resources with
--
-- -   __@OTHER-ACCOUNTS@__ – principals that are sharing resources with
--     your account
listPrincipals_resourceOwner :: Lens.Lens' ListPrincipals ResourceOwner
listPrincipals_resourceOwner = Lens.lens (\ListPrincipals' {resourceOwner} -> resourceOwner) (\s@ListPrincipals' {} a -> s {resourceOwner = a} :: ListPrincipals)

instance Core.AWSPager ListPrincipals where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPrincipalsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listPrincipalsResponse_principals
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPrincipals_nextToken
          Lens..~ rs
          Lens.^? listPrincipalsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListPrincipals where
  type
    AWSResponse ListPrincipals =
      ListPrincipalsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPrincipalsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "principals" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListPrincipals where
  hashWithSalt _salt ListPrincipals' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` principals
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resourceShareArns
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceOwner

instance Prelude.NFData ListPrincipals where
  rnf ListPrincipals' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceShareArns
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceOwner

instance Data.ToHeaders ListPrincipals where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPrincipals where
  toJSON ListPrincipals' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceType" Data..=) Prelude.<$> resourceType,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("principals" Data..=) Prelude.<$> principals,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("resourceShareArns" Data..=)
              Prelude.<$> resourceShareArns,
            ("resourceArn" Data..=) Prelude.<$> resourceArn,
            Prelude.Just
              ("resourceOwner" Data..= resourceOwner)
          ]
      )

instance Data.ToPath ListPrincipals where
  toPath = Prelude.const "/listprincipals"

instance Data.ToQuery ListPrincipals where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPrincipalsResponse' smart constructor.
data ListPrincipalsResponse = ListPrincipalsResponse'
  { -- | If present, this value indicates that more output is available than is
    -- included in the current response. Use this value in the @NextToken@
    -- request parameter in a subsequent call to the operation to get the next
    -- part of the output. You should repeat this until the @NextToken@
    -- response element comes back as @null@. This indicates that this is the
    -- last page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that contain the details about the principals.
    principals :: Prelude.Maybe [Principal],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPrincipalsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPrincipalsResponse_nextToken' - If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
--
-- 'principals', 'listPrincipalsResponse_principals' - An array of objects that contain the details about the principals.
--
-- 'httpStatus', 'listPrincipalsResponse_httpStatus' - The response's http status code.
newListPrincipalsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPrincipalsResponse
newListPrincipalsResponse pHttpStatus_ =
  ListPrincipalsResponse'
    { nextToken =
        Prelude.Nothing,
      principals = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If present, this value indicates that more output is available than is
-- included in the current response. Use this value in the @NextToken@
-- request parameter in a subsequent call to the operation to get the next
-- part of the output. You should repeat this until the @NextToken@
-- response element comes back as @null@. This indicates that this is the
-- last page of results.
listPrincipalsResponse_nextToken :: Lens.Lens' ListPrincipalsResponse (Prelude.Maybe Prelude.Text)
listPrincipalsResponse_nextToken = Lens.lens (\ListPrincipalsResponse' {nextToken} -> nextToken) (\s@ListPrincipalsResponse' {} a -> s {nextToken = a} :: ListPrincipalsResponse)

-- | An array of objects that contain the details about the principals.
listPrincipalsResponse_principals :: Lens.Lens' ListPrincipalsResponse (Prelude.Maybe [Principal])
listPrincipalsResponse_principals = Lens.lens (\ListPrincipalsResponse' {principals} -> principals) (\s@ListPrincipalsResponse' {} a -> s {principals = a} :: ListPrincipalsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listPrincipalsResponse_httpStatus :: Lens.Lens' ListPrincipalsResponse Prelude.Int
listPrincipalsResponse_httpStatus = Lens.lens (\ListPrincipalsResponse' {httpStatus} -> httpStatus) (\s@ListPrincipalsResponse' {} a -> s {httpStatus = a} :: ListPrincipalsResponse)

instance Prelude.NFData ListPrincipalsResponse where
  rnf ListPrincipalsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf principals
      `Prelude.seq` Prelude.rnf httpStatus
