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
-- Module      : Amazonka.ResilienceHub.ListAppVersionResourceMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists how the resources in an application version are mapped\/sourced
-- from. Mappings can be physical resource identifiers, CloudFormation
-- stacks, resource-groups, or an application registry app.
module Amazonka.ResilienceHub.ListAppVersionResourceMappings
  ( -- * Creating a Request
    ListAppVersionResourceMappings (..),
    newListAppVersionResourceMappings,

    -- * Request Lenses
    listAppVersionResourceMappings_maxResults,
    listAppVersionResourceMappings_nextToken,
    listAppVersionResourceMappings_appArn,
    listAppVersionResourceMappings_appVersion,

    -- * Destructuring the Response
    ListAppVersionResourceMappingsResponse (..),
    newListAppVersionResourceMappingsResponse,

    -- * Response Lenses
    listAppVersionResourceMappingsResponse_nextToken,
    listAppVersionResourceMappingsResponse_httpStatus,
    listAppVersionResourceMappingsResponse_resourceMappings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppVersionResourceMappings' smart constructor.
data ListAppVersionResourceMappings = ListAppVersionResourceMappings'
  { -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Null, or the token from a previous call to get the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application. The format for this
    -- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/.
    appArn :: Prelude.Text,
    -- | The version of the application.
    appVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionResourceMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppVersionResourceMappings_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'nextToken', 'listAppVersionResourceMappings_nextToken' - Null, or the token from a previous call to get the next set of results.
--
-- 'appArn', 'listAppVersionResourceMappings_appArn' - The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
--
-- 'appVersion', 'listAppVersionResourceMappings_appVersion' - The version of the application.
newListAppVersionResourceMappings ::
  -- | 'appArn'
  Prelude.Text ->
  -- | 'appVersion'
  Prelude.Text ->
  ListAppVersionResourceMappings
newListAppVersionResourceMappings
  pAppArn_
  pAppVersion_ =
    ListAppVersionResourceMappings'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        appArn = pAppArn_,
        appVersion = pAppVersion_
      }

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
listAppVersionResourceMappings_maxResults :: Lens.Lens' ListAppVersionResourceMappings (Prelude.Maybe Prelude.Natural)
listAppVersionResourceMappings_maxResults = Lens.lens (\ListAppVersionResourceMappings' {maxResults} -> maxResults) (\s@ListAppVersionResourceMappings' {} a -> s {maxResults = a} :: ListAppVersionResourceMappings)

-- | Null, or the token from a previous call to get the next set of results.
listAppVersionResourceMappings_nextToken :: Lens.Lens' ListAppVersionResourceMappings (Prelude.Maybe Prelude.Text)
listAppVersionResourceMappings_nextToken = Lens.lens (\ListAppVersionResourceMappings' {nextToken} -> nextToken) (\s@ListAppVersionResourceMappings' {} a -> s {nextToken = a} :: ListAppVersionResourceMappings)

-- | The Amazon Resource Name (ARN) of the application. The format for this
-- ARN is: arn:@partition@:resiliencehub:@region@:@account@:app\/@app-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/.
listAppVersionResourceMappings_appArn :: Lens.Lens' ListAppVersionResourceMappings Prelude.Text
listAppVersionResourceMappings_appArn = Lens.lens (\ListAppVersionResourceMappings' {appArn} -> appArn) (\s@ListAppVersionResourceMappings' {} a -> s {appArn = a} :: ListAppVersionResourceMappings)

-- | The version of the application.
listAppVersionResourceMappings_appVersion :: Lens.Lens' ListAppVersionResourceMappings Prelude.Text
listAppVersionResourceMappings_appVersion = Lens.lens (\ListAppVersionResourceMappings' {appVersion} -> appVersion) (\s@ListAppVersionResourceMappings' {} a -> s {appVersion = a} :: ListAppVersionResourceMappings)

instance
  Core.AWSRequest
    ListAppVersionResourceMappings
  where
  type
    AWSResponse ListAppVersionResourceMappings =
      ListAppVersionResourceMappingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppVersionResourceMappingsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "resourceMappings"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListAppVersionResourceMappings
  where
  hashWithSalt
    _salt
    ListAppVersionResourceMappings' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` appArn
        `Prelude.hashWithSalt` appVersion

instance
  Prelude.NFData
    ListAppVersionResourceMappings
  where
  rnf ListAppVersionResourceMappings' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appArn
      `Prelude.seq` Prelude.rnf appVersion

instance
  Data.ToHeaders
    ListAppVersionResourceMappings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAppVersionResourceMappings where
  toJSON ListAppVersionResourceMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("appArn" Data..= appArn),
            Prelude.Just ("appVersion" Data..= appVersion)
          ]
      )

instance Data.ToPath ListAppVersionResourceMappings where
  toPath =
    Prelude.const "/list-app-version-resource-mappings"

instance Data.ToQuery ListAppVersionResourceMappings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAppVersionResourceMappingsResponse' smart constructor.
data ListAppVersionResourceMappingsResponse = ListAppVersionResourceMappingsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Mappings used to map logical resources from the template to physical
    -- resources. You can use the mapping type @CFN_STACK@ if the application
    -- template uses a logical stack name. Or you can map individual resources
    -- by using the mapping type @RESOURCE@. We recommend using the mapping
    -- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
    resourceMappings :: [ResourceMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppVersionResourceMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppVersionResourceMappingsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'httpStatus', 'listAppVersionResourceMappingsResponse_httpStatus' - The response's http status code.
--
-- 'resourceMappings', 'listAppVersionResourceMappingsResponse_resourceMappings' - Mappings used to map logical resources from the template to physical
-- resources. You can use the mapping type @CFN_STACK@ if the application
-- template uses a logical stack name. Or you can map individual resources
-- by using the mapping type @RESOURCE@. We recommend using the mapping
-- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
newListAppVersionResourceMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppVersionResourceMappingsResponse
newListAppVersionResourceMappingsResponse
  pHttpStatus_ =
    ListAppVersionResourceMappingsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        resourceMappings = Prelude.mempty
      }

-- | The token for the next set of results, or null if there are no more
-- results.
listAppVersionResourceMappingsResponse_nextToken :: Lens.Lens' ListAppVersionResourceMappingsResponse (Prelude.Maybe Prelude.Text)
listAppVersionResourceMappingsResponse_nextToken = Lens.lens (\ListAppVersionResourceMappingsResponse' {nextToken} -> nextToken) (\s@ListAppVersionResourceMappingsResponse' {} a -> s {nextToken = a} :: ListAppVersionResourceMappingsResponse)

-- | The response's http status code.
listAppVersionResourceMappingsResponse_httpStatus :: Lens.Lens' ListAppVersionResourceMappingsResponse Prelude.Int
listAppVersionResourceMappingsResponse_httpStatus = Lens.lens (\ListAppVersionResourceMappingsResponse' {httpStatus} -> httpStatus) (\s@ListAppVersionResourceMappingsResponse' {} a -> s {httpStatus = a} :: ListAppVersionResourceMappingsResponse)

-- | Mappings used to map logical resources from the template to physical
-- resources. You can use the mapping type @CFN_STACK@ if the application
-- template uses a logical stack name. Or you can map individual resources
-- by using the mapping type @RESOURCE@. We recommend using the mapping
-- type @CFN_STACK@ if the application is backed by a CloudFormation stack.
listAppVersionResourceMappingsResponse_resourceMappings :: Lens.Lens' ListAppVersionResourceMappingsResponse [ResourceMapping]
listAppVersionResourceMappingsResponse_resourceMappings = Lens.lens (\ListAppVersionResourceMappingsResponse' {resourceMappings} -> resourceMappings) (\s@ListAppVersionResourceMappingsResponse' {} a -> s {resourceMappings = a} :: ListAppVersionResourceMappingsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAppVersionResourceMappingsResponse
  where
  rnf ListAppVersionResourceMappingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf resourceMappings
