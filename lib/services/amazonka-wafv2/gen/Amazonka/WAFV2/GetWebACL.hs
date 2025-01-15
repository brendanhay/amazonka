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
-- Module      : Amazonka.WAFV2.GetWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified WebACL.
module Amazonka.WAFV2.GetWebACL
  ( -- * Creating a Request
    GetWebACL (..),
    newGetWebACL,

    -- * Request Lenses
    getWebACL_name,
    getWebACL_scope,
    getWebACL_id,

    -- * Destructuring the Response
    GetWebACLResponse (..),
    newGetWebACLResponse,

    -- * Response Lenses
    getWebACLResponse_applicationIntegrationURL,
    getWebACLResponse_lockToken,
    getWebACLResponse_webACL,
    getWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetWebACL' smart constructor.
data GetWebACL = GetWebACL'
  { -- | The name of the web ACL. You cannot change the name of a web ACL after
    -- you create it.
    name :: Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope,
    -- | The unique identifier for the web ACL. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getWebACL_name' - The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
--
-- 'scope', 'getWebACL_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
--
-- 'id', 'getWebACL_id' - The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
newGetWebACL ::
  -- | 'name'
  Prelude.Text ->
  -- | 'scope'
  Scope ->
  -- | 'id'
  Prelude.Text ->
  GetWebACL
newGetWebACL pName_ pScope_ pId_ =
  GetWebACL'
    { name = pName_,
      scope = pScope_,
      id = pId_
    }

-- | The name of the web ACL. You cannot change the name of a web ACL after
-- you create it.
getWebACL_name :: Lens.Lens' GetWebACL Prelude.Text
getWebACL_name = Lens.lens (\GetWebACL' {name} -> name) (\s@GetWebACL' {} a -> s {name = a} :: GetWebACL)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
getWebACL_scope :: Lens.Lens' GetWebACL Scope
getWebACL_scope = Lens.lens (\GetWebACL' {scope} -> scope) (\s@GetWebACL' {} a -> s {scope = a} :: GetWebACL)

-- | The unique identifier for the web ACL. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
getWebACL_id :: Lens.Lens' GetWebACL Prelude.Text
getWebACL_id = Lens.lens (\GetWebACL' {id} -> id) (\s@GetWebACL' {} a -> s {id = a} :: GetWebACL)

instance Core.AWSRequest GetWebACL where
  type AWSResponse GetWebACL = GetWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWebACLResponse'
            Prelude.<$> (x Data..?> "ApplicationIntegrationURL")
            Prelude.<*> (x Data..?> "LockToken")
            Prelude.<*> (x Data..?> "WebACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWebACL where
  hashWithSalt _salt GetWebACL' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetWebACL where
  rnf GetWebACL' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf scope `Prelude.seq`
        Prelude.rnf id

instance Data.ToHeaders GetWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSWAF_20190729.GetWebACL" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWebACL where
  toJSON GetWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("Id" Data..= id)
          ]
      )

instance Data.ToPath GetWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWebACLResponse' smart constructor.
data GetWebACLResponse = GetWebACLResponse'
  { -- | The URL to use in SDK integrations with Amazon Web Services managed rule
    -- groups. For example, you can use the integration SDKs with the account
    -- takeover prevention managed rule group @AWSManagedRulesATPRuleSet@. This
    -- is only populated if you are using a rule group in your web ACL that
    -- integrates with your applications in this way. For more information, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
    -- in the /WAF Developer Guide/.
    applicationIntegrationURL :: Prelude.Maybe Prelude.Text,
    -- | A token used for optimistic locking. WAF returns a token to your @get@
    -- and @list@ requests, to mark the state of the entity at the time of the
    -- request. To make changes to the entity associated with the token, you
    -- provide the token to operations like @update@ and @delete@. WAF uses the
    -- token to ensure that no changes have been made to the entity since you
    -- last retrieved it. If a change has been made, the update fails with a
    -- @WAFOptimisticLockException@. If this happens, perform another @get@,
    -- and use the new token returned by that operation.
    lockToken :: Prelude.Maybe Prelude.Text,
    -- | The web ACL specification. You can modify the settings in this web ACL
    -- and use it to update this web ACL or create a new one.
    webACL :: Prelude.Maybe WebACL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationIntegrationURL', 'getWebACLResponse_applicationIntegrationURL' - The URL to use in SDK integrations with Amazon Web Services managed rule
-- groups. For example, you can use the integration SDKs with the account
-- takeover prevention managed rule group @AWSManagedRulesATPRuleSet@. This
-- is only populated if you are using a rule group in your web ACL that
-- integrates with your applications in this way. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
--
-- 'lockToken', 'getWebACLResponse_lockToken' - A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
--
-- 'webACL', 'getWebACLResponse_webACL' - The web ACL specification. You can modify the settings in this web ACL
-- and use it to update this web ACL or create a new one.
--
-- 'httpStatus', 'getWebACLResponse_httpStatus' - The response's http status code.
newGetWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWebACLResponse
newGetWebACLResponse pHttpStatus_ =
  GetWebACLResponse'
    { applicationIntegrationURL =
        Prelude.Nothing,
      lockToken = Prelude.Nothing,
      webACL = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL to use in SDK integrations with Amazon Web Services managed rule
-- groups. For example, you can use the integration SDKs with the account
-- takeover prevention managed rule group @AWSManagedRulesATPRuleSet@. This
-- is only populated if you are using a rule group in your web ACL that
-- integrates with your applications in this way. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
getWebACLResponse_applicationIntegrationURL :: Lens.Lens' GetWebACLResponse (Prelude.Maybe Prelude.Text)
getWebACLResponse_applicationIntegrationURL = Lens.lens (\GetWebACLResponse' {applicationIntegrationURL} -> applicationIntegrationURL) (\s@GetWebACLResponse' {} a -> s {applicationIntegrationURL = a} :: GetWebACLResponse)

-- | A token used for optimistic locking. WAF returns a token to your @get@
-- and @list@ requests, to mark the state of the entity at the time of the
-- request. To make changes to the entity associated with the token, you
-- provide the token to operations like @update@ and @delete@. WAF uses the
-- token to ensure that no changes have been made to the entity since you
-- last retrieved it. If a change has been made, the update fails with a
-- @WAFOptimisticLockException@. If this happens, perform another @get@,
-- and use the new token returned by that operation.
getWebACLResponse_lockToken :: Lens.Lens' GetWebACLResponse (Prelude.Maybe Prelude.Text)
getWebACLResponse_lockToken = Lens.lens (\GetWebACLResponse' {lockToken} -> lockToken) (\s@GetWebACLResponse' {} a -> s {lockToken = a} :: GetWebACLResponse)

-- | The web ACL specification. You can modify the settings in this web ACL
-- and use it to update this web ACL or create a new one.
getWebACLResponse_webACL :: Lens.Lens' GetWebACLResponse (Prelude.Maybe WebACL)
getWebACLResponse_webACL = Lens.lens (\GetWebACLResponse' {webACL} -> webACL) (\s@GetWebACLResponse' {} a -> s {webACL = a} :: GetWebACLResponse)

-- | The response's http status code.
getWebACLResponse_httpStatus :: Lens.Lens' GetWebACLResponse Prelude.Int
getWebACLResponse_httpStatus = Lens.lens (\GetWebACLResponse' {httpStatus} -> httpStatus) (\s@GetWebACLResponse' {} a -> s {httpStatus = a} :: GetWebACLResponse)

instance Prelude.NFData GetWebACLResponse where
  rnf GetWebACLResponse' {..} =
    Prelude.rnf applicationIntegrationURL `Prelude.seq`
      Prelude.rnf lockToken `Prelude.seq`
        Prelude.rnf webACL `Prelude.seq`
          Prelude.rnf httpStatus
