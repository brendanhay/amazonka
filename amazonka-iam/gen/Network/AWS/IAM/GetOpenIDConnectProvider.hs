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
    -- in the /AWS General Reference/.
    openIDConnectProviderArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS General Reference/.
newGetOpenIDConnectProvider ::
  -- | 'openIDConnectProviderArn'
  Core.Text ->
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
-- in the /AWS General Reference/.
getOpenIDConnectProvider_openIDConnectProviderArn :: Lens.Lens' GetOpenIDConnectProvider Core.Text
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
            Core.<$> ( x Core..@? "ClientIDList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "CreateDate")
            Core.<*> ( x Core..@? "ThumbprintList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> ( x Core..@? "Tags" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Url")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOpenIDConnectProvider

instance Core.NFData GetOpenIDConnectProvider

instance Core.ToHeaders GetOpenIDConnectProvider where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetOpenIDConnectProvider where
  toPath = Core.const "/"

instance Core.ToQuery GetOpenIDConnectProvider where
  toQuery GetOpenIDConnectProvider' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("GetOpenIDConnectProvider" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
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
    clientIDList :: Core.Maybe [Core.Text],
    -- | The date and time when the IAM OIDC provider resource object was created
    -- in the AWS account.
    createDate :: Core.Maybe Core.ISO8601,
    -- | A list of certificate thumbprints that are associated with the specified
    -- IAM OIDC provider resource object. For more information, see
    -- CreateOpenIDConnectProvider.
    thumbprintList :: Core.Maybe [Core.Text],
    -- | A list of tags that are attached to the specified IAM OIDC provider. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Core.Maybe [Tag],
    -- | The URL that the IAM OIDC provider resource object is associated with.
    -- For more information, see CreateOpenIDConnectProvider.
    url :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the AWS account.
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
  Core.Int ->
  GetOpenIDConnectProviderResponse
newGetOpenIDConnectProviderResponse pHttpStatus_ =
  GetOpenIDConnectProviderResponse'
    { clientIDList =
        Core.Nothing,
      createDate = Core.Nothing,
      thumbprintList = Core.Nothing,
      tags = Core.Nothing,
      url = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of client IDs (also known as audiences) that are associated with
-- the specified IAM OIDC provider resource object. For more information,
-- see CreateOpenIDConnectProvider.
getOpenIDConnectProviderResponse_clientIDList :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe [Core.Text])
getOpenIDConnectProviderResponse_clientIDList = Lens.lens (\GetOpenIDConnectProviderResponse' {clientIDList} -> clientIDList) (\s@GetOpenIDConnectProviderResponse' {} a -> s {clientIDList = a} :: GetOpenIDConnectProviderResponse) Core.. Lens.mapping Lens._Coerce

-- | The date and time when the IAM OIDC provider resource object was created
-- in the AWS account.
getOpenIDConnectProviderResponse_createDate :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe Core.UTCTime)
getOpenIDConnectProviderResponse_createDate = Lens.lens (\GetOpenIDConnectProviderResponse' {createDate} -> createDate) (\s@GetOpenIDConnectProviderResponse' {} a -> s {createDate = a} :: GetOpenIDConnectProviderResponse) Core.. Lens.mapping Core._Time

-- | A list of certificate thumbprints that are associated with the specified
-- IAM OIDC provider resource object. For more information, see
-- CreateOpenIDConnectProvider.
getOpenIDConnectProviderResponse_thumbprintList :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe [Core.Text])
getOpenIDConnectProviderResponse_thumbprintList = Lens.lens (\GetOpenIDConnectProviderResponse' {thumbprintList} -> thumbprintList) (\s@GetOpenIDConnectProviderResponse' {} a -> s {thumbprintList = a} :: GetOpenIDConnectProviderResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of tags that are attached to the specified IAM OIDC provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
getOpenIDConnectProviderResponse_tags :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe [Tag])
getOpenIDConnectProviderResponse_tags = Lens.lens (\GetOpenIDConnectProviderResponse' {tags} -> tags) (\s@GetOpenIDConnectProviderResponse' {} a -> s {tags = a} :: GetOpenIDConnectProviderResponse) Core.. Lens.mapping Lens._Coerce

-- | The URL that the IAM OIDC provider resource object is associated with.
-- For more information, see CreateOpenIDConnectProvider.
getOpenIDConnectProviderResponse_url :: Lens.Lens' GetOpenIDConnectProviderResponse (Core.Maybe Core.Text)
getOpenIDConnectProviderResponse_url = Lens.lens (\GetOpenIDConnectProviderResponse' {url} -> url) (\s@GetOpenIDConnectProviderResponse' {} a -> s {url = a} :: GetOpenIDConnectProviderResponse)

-- | The response's http status code.
getOpenIDConnectProviderResponse_httpStatus :: Lens.Lens' GetOpenIDConnectProviderResponse Core.Int
getOpenIDConnectProviderResponse_httpStatus = Lens.lens (\GetOpenIDConnectProviderResponse' {httpStatus} -> httpStatus) (\s@GetOpenIDConnectProviderResponse' {} a -> s {httpStatus = a} :: GetOpenIDConnectProviderResponse)

instance Core.NFData GetOpenIDConnectProviderResponse
