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
-- Module      : Network.AWS.Greengrass.CreateGroupCertificateAuthority
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a CA for the group. If a CA already exists, it will rotate the
-- existing CA.
module Network.AWS.Greengrass.CreateGroupCertificateAuthority
  ( -- * Creating a Request
    CreateGroupCertificateAuthority (..),
    newCreateGroupCertificateAuthority,

    -- * Request Lenses
    createGroupCertificateAuthority_amznClientToken,
    createGroupCertificateAuthority_groupId,

    -- * Destructuring the Response
    CreateGroupCertificateAuthorityResponse (..),
    newCreateGroupCertificateAuthorityResponse,

    -- * Response Lenses
    createGroupCertificateAuthorityResponse_groupCertificateAuthorityArn,
    createGroupCertificateAuthorityResponse_httpStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateGroupCertificateAuthority' smart constructor.
data CreateGroupCertificateAuthority = CreateGroupCertificateAuthority'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupCertificateAuthority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amznClientToken', 'createGroupCertificateAuthority_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'groupId', 'createGroupCertificateAuthority_groupId' - The ID of the Greengrass group.
newCreateGroupCertificateAuthority ::
  -- | 'groupId'
  Prelude.Text ->
  CreateGroupCertificateAuthority
newCreateGroupCertificateAuthority pGroupId_ =
  CreateGroupCertificateAuthority'
    { amznClientToken =
        Prelude.Nothing,
      groupId = pGroupId_
    }

-- | A client token used to correlate requests and responses.
createGroupCertificateAuthority_amznClientToken :: Lens.Lens' CreateGroupCertificateAuthority (Prelude.Maybe Prelude.Text)
createGroupCertificateAuthority_amznClientToken = Lens.lens (\CreateGroupCertificateAuthority' {amznClientToken} -> amznClientToken) (\s@CreateGroupCertificateAuthority' {} a -> s {amznClientToken = a} :: CreateGroupCertificateAuthority)

-- | The ID of the Greengrass group.
createGroupCertificateAuthority_groupId :: Lens.Lens' CreateGroupCertificateAuthority Prelude.Text
createGroupCertificateAuthority_groupId = Lens.lens (\CreateGroupCertificateAuthority' {groupId} -> groupId) (\s@CreateGroupCertificateAuthority' {} a -> s {groupId = a} :: CreateGroupCertificateAuthority)

instance
  Prelude.AWSRequest
    CreateGroupCertificateAuthority
  where
  type
    Rs CreateGroupCertificateAuthority =
      CreateGroupCertificateAuthorityResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateGroupCertificateAuthorityResponse'
            Prelude.<$> (x Prelude..?> "GroupCertificateAuthorityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateGroupCertificateAuthority

instance
  Prelude.NFData
    CreateGroupCertificateAuthority

instance
  Prelude.ToHeaders
    CreateGroupCertificateAuthority
  where
  toHeaders CreateGroupCertificateAuthority' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Prelude.=# amznClientToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance
  Prelude.ToJSON
    CreateGroupCertificateAuthority
  where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance
  Prelude.ToPath
    CreateGroupCertificateAuthority
  where
  toPath CreateGroupCertificateAuthority' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/certificateauthorities"
      ]

instance
  Prelude.ToQuery
    CreateGroupCertificateAuthority
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateGroupCertificateAuthorityResponse' smart constructor.
data CreateGroupCertificateAuthorityResponse = CreateGroupCertificateAuthorityResponse'
  { -- | The ARN of the group certificate authority.
    groupCertificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateGroupCertificateAuthorityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupCertificateAuthorityArn', 'createGroupCertificateAuthorityResponse_groupCertificateAuthorityArn' - The ARN of the group certificate authority.
--
-- 'httpStatus', 'createGroupCertificateAuthorityResponse_httpStatus' - The response's http status code.
newCreateGroupCertificateAuthorityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateGroupCertificateAuthorityResponse
newCreateGroupCertificateAuthorityResponse
  pHttpStatus_ =
    CreateGroupCertificateAuthorityResponse'
      { groupCertificateAuthorityArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ARN of the group certificate authority.
createGroupCertificateAuthorityResponse_groupCertificateAuthorityArn :: Lens.Lens' CreateGroupCertificateAuthorityResponse (Prelude.Maybe Prelude.Text)
createGroupCertificateAuthorityResponse_groupCertificateAuthorityArn = Lens.lens (\CreateGroupCertificateAuthorityResponse' {groupCertificateAuthorityArn} -> groupCertificateAuthorityArn) (\s@CreateGroupCertificateAuthorityResponse' {} a -> s {groupCertificateAuthorityArn = a} :: CreateGroupCertificateAuthorityResponse)

-- | The response's http status code.
createGroupCertificateAuthorityResponse_httpStatus :: Lens.Lens' CreateGroupCertificateAuthorityResponse Prelude.Int
createGroupCertificateAuthorityResponse_httpStatus = Lens.lens (\CreateGroupCertificateAuthorityResponse' {httpStatus} -> httpStatus) (\s@CreateGroupCertificateAuthorityResponse' {} a -> s {httpStatus = a} :: CreateGroupCertificateAuthorityResponse)

instance
  Prelude.NFData
    CreateGroupCertificateAuthorityResponse
