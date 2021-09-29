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
-- Module      : Network.AWS.IAM.GetOpenIDConnectProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified OpenID Connect (OIDC) provider
-- resource object in IAM.
module Network.AWS.IAM.GetOpenIDConnectProvider
  ( -- * Creating a Request
    GetOpenIDConnectProvider (..),
    newGetOpenIDConnectProvider,

    -- * Request Lenses
    getOpenIDConnectProvider_openIDConnectProviderArn,

    -- * Destructuring the Response
    GetOpenIDConnectProviderResponse (..),
    newGetOpenIDConnectProviderResponse,

    -- * Response Lenses
    getOpenIDConnectProviderResponse_clientIDList,
    getOpenIDConnectProviderResponse_createDate,
    getOpenIDConnectProviderResponse_thumbprintList,
    getOpenIDConnectProviderResponse_tags,
    getOpenIDConnectProviderResponse_url,
    getOpenIDConnectProviderResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOpenIDConnectProvider' smart constructor.
data GetOpenIDConnectProvider = GetOpenIDConnectProvider'
  { -- | The Amazon Resource Name (ARN) of the OIDC provider resource object in
    -- IAM to get information for. You can get a list of OIDC provider resource
    -- ARNs by using the ListOpenIDConnectProviders operation.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /Amazon Web Services General Reference/.
    openIDConnectProviderArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpenIDConnectProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderArn', 'getOpenIDConnectProvider_openIDConnectProviderArn' - The Amazon Resource Name (ARN) of the OIDC provider resource object in
-- IAM to get information for. You can get a list of OIDC provider resource
-- ARNs by using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
newGetOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Prelude.Text ->
  GetOpenIDConnectProvider
newGetOpenIDConnectProvider
  pOpenIDConnectProviderArn_ =
    GetOpenIDConnectProvider'
      { openIDConnectProviderArn =
          pOpenIDConnectProviderArn_
      }

-- | The Amazon Resource Name (ARN) of the OIDC provider resource object in
-- IAM to get information for. You can get a list of OIDC provider resource
-- ARNs by using the ListOpenIDConnectProviders operation.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /Amazon Web Services General Reference/.
getOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' GetOpenIDConnectProvider Prelude.Text
getOpenIDConnectProvider_openIDConnectProviderArn = Lens.lens (\GetOpenIDConnectProvider' {openIDConnectProviderArn} -> openIDConnectProviderArn) (\s@GetOpenIDConnectProvider' {} a -> s {openIDConnectProviderArn = a} :: GetOpenIDConnectProvider)

instance Core.AWSRequest GetOpenIDConnectProvider where
  type
    AWSResponse GetOpenIDConnectProvider =
      GetOpenIDConnectProviderResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetOpenIDConnectProviderResult"
      ( \s h x ->
          GetOpenIDConnectProviderResponse'
            Prelude.<$> ( x Core..@? "ClientIDList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "CreateDate")
            Prelude.<*> ( x Core..@? "ThumbprintList" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOpenIDConnectProvider

instance Prelude.NFData GetOpenIDConnectProvider

instance Core.ToHeaders GetOpenIDConnectProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetOpenIDConnectProvider where
  toPath = Prelude.const "/"

instance Core.ToQuery GetOpenIDConnectProvider where
  toQuery GetOpenIDConnectProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetOpenIDConnectProvider" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "OpenIDConnectProviderArn"
          Core.=: openIDConnectProviderArn
      ]

-- | Contains the response to a successful GetOpenIDConnectProvider request.
--
-- /See:/ 'newGetOpenIDConnectProviderResponse' smart constructor.
data GetOpenIDConnectProviderResponse = GetOpenIDConnectProviderResponse'
  { -- | A list of client IDs (also known as audiences) that are associated with
    -- the specified IAM OIDC provider resource object. For more information,
    -- see CreateOpenIDConnectProvider.
    clientIDList :: Prelude.Maybe [Prelude.Text],
    -- | The date and time when the IAM OIDC provider resource object was created
    -- in the Amazon Web Services account.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | A list of certificate thumbprints that are associated with the specified
    -- IAM OIDC provider resource object. For more information, see
    -- CreateOpenIDConnectProvider.
    thumbprintList :: Prelude.Maybe [Prelude.Text],
    -- | A list of tags that are attached to the specified IAM OIDC provider. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The URL that the IAM OIDC provider resource object is associated with.
    -- For more information, see CreateOpenIDConnectProvider.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOpenIDConnectProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientIDList', 'getOpenIDConnectProviderResponse_clientIDList' - A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OIDC provider resource object. For more information,
-- see CreateOpenIDConnectProvider.
--
-- 'createDate', 'getOpenIDConnectProviderResponse_createDate' - The date and time when the IAM OIDC provider resource object was created
-- in the Amazon Web Services account.
--
-- 'thumbprintList', 'getOpenIDConnectProviderResponse_thumbprintList' - A list of certificate thumbprints that are associated with the specified
-- IAM OIDC provider resource object. For more information, see
-- CreateOpenIDConnectProvider.
--
-- 'tags', 'getOpenIDConnectProviderResponse_tags' - A list of tags that are attached to the specified IAM OIDC provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'url', 'getOpenIDConnectProviderResponse_url' - The URL that the IAM OIDC provider resource object is associated with.
-- For more information, see CreateOpenIDConnectProvider.
--
-- 'httpStatus', 'getOpenIDConnectProviderResponse_httpStatus' - The response's http status code.
newGetOpenIDConnectProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOpenIDConnectProviderResponse
newGetOpenIDConnectProviderResponse pHttpStatus_ =
  GetOpenIDConnectProviderResponse'
    { clientIDList =
        Prelude.Nothing,
      createDate = Prelude.Nothing,
      thumbprintList = Prelude.Nothing,
      tags = Prelude.Nothing,
      url = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OIDC provider resource object. For more information,
-- see CreateOpenIDConnectProvider.
getOpenIDConnectProviderResponse_clientIDList :: Lens.Lens' GetOpenIDConnectProviderResponse (Prelude.Maybe [Prelude.Text])
getOpenIDConnectProviderResponse_clientIDList = Lens.lens (\GetOpenIDConnectProviderResponse' {clientIDList} -> clientIDList) (\s@GetOpenIDConnectProviderResponse' {} a -> s {clientIDList = a} :: GetOpenIDConnectProviderResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The date and time when the IAM OIDC provider resource object was created
-- in the Amazon Web Services account.
getOpenIDConnectProviderResponse_createDate :: Lens.Lens' GetOpenIDConnectProviderResponse (Prelude.Maybe Prelude.UTCTime)
getOpenIDConnectProviderResponse_createDate = Lens.lens (\GetOpenIDConnectProviderResponse' {createDate} -> createDate) (\s@GetOpenIDConnectProviderResponse' {} a -> s {createDate = a} :: GetOpenIDConnectProviderResponse) Prelude.. Lens.mapping Core._Time

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OIDC provider resource object. For more information, see
-- CreateOpenIDConnectProvider.
getOpenIDConnectProviderResponse_thumbprintList :: Lens.Lens' GetOpenIDConnectProviderResponse (Prelude.Maybe [Prelude.Text])
getOpenIDConnectProviderResponse_thumbprintList = Lens.lens (\GetOpenIDConnectProviderResponse' {thumbprintList} -> thumbprintList) (\s@GetOpenIDConnectProviderResponse' {} a -> s {thumbprintList = a} :: GetOpenIDConnectProviderResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of tags that are attached to the specified IAM OIDC provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
getOpenIDConnectProviderResponse_tags :: Lens.Lens' GetOpenIDConnectProviderResponse (Prelude.Maybe [Tag])
getOpenIDConnectProviderResponse_tags = Lens.lens (\GetOpenIDConnectProviderResponse' {tags} -> tags) (\s@GetOpenIDConnectProviderResponse' {} a -> s {tags = a} :: GetOpenIDConnectProviderResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The URL that the IAM OIDC provider resource object is associated with.
-- For more information, see CreateOpenIDConnectProvider.
getOpenIDConnectProviderResponse_url :: Lens.Lens' GetOpenIDConnectProviderResponse (Prelude.Maybe Prelude.Text)
getOpenIDConnectProviderResponse_url = Lens.lens (\GetOpenIDConnectProviderResponse' {url} -> url) (\s@GetOpenIDConnectProviderResponse' {} a -> s {url = a} :: GetOpenIDConnectProviderResponse)

-- | The response's http status code.
getOpenIDConnectProviderResponse_httpStatus :: Lens.Lens' GetOpenIDConnectProviderResponse Prelude.Int
getOpenIDConnectProviderResponse_httpStatus = Lens.lens (\GetOpenIDConnectProviderResponse' {httpStatus} -> httpStatus) (\s@GetOpenIDConnectProviderResponse' {} a -> s {httpStatus = a} :: GetOpenIDConnectProviderResponse)

instance
  Prelude.NFData
    GetOpenIDConnectProviderResponse
